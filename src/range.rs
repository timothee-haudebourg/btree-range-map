use crate::util::Measure;
use std::{
	cmp::PartialOrd,
	ops::{Bound, RangeBounds},
};

mod bound;
mod ordering;

mod any;
mod from_excluded;
mod from_excluded_to;
mod from_excluded_to_included;

pub use any::*;
pub use bound::*;
pub use from_excluded::*;
pub use from_excluded_to::*;
pub use from_excluded_to_included::*;
pub use ordering::*;

/// Types that can be interpreted as ranges.
pub trait AsRange: Sized {
	/// Type of the elements of the range.
	type Item: Measure + PartialOrd;

	/// Start bound of the range.
	fn start(&self) -> Bound<&Self::Item>;

	/// End bound of the range.
	fn end(&self) -> Bound<&Self::Item>;

	fn is_empty(&self) -> bool {
		is_range_empty(self.start(), self.end())
	}

	fn intersects<R: AsRange>(&self, other: &R) -> bool
	where
		Self::Item: PartialOrd<R::Item> + Measure<R::Item>,
	{
		matches!(
			self.range_partial_cmp(other),
			Some(RangeOrdering::Intersecting(_, _))
		)
	}

	fn connected_to<R: AsRange>(&self, other: &R) -> bool
	where
		Self::Item: PartialOrd<R::Item> + Measure<R::Item>,
	{
		match self.range_partial_cmp(other) {
			Some(RangeOrdering::Intersecting(_, _)) => true,
			Some(RangeOrdering::Before(connected)) => connected,
			Some(RangeOrdering::After(connected)) => connected,
			_ => false,
		}
	}

	fn intersected_with<'a, R: AsRange<Item = Self::Item>>(
		&'a self,
		other: &'a R,
	) -> AnyRange<&'a Self::Item>
	where
		Self::Item: PartialOrd + Measure,
	{
		AnyRange {
			start: max_bound(self.start(), other.start(), true),
			end: min_bound(self.end(), other.end(), false),
		}
	}

	fn without<'a, R: AsRange<Item = Self::Item>>(
		&'a self,
		other: &'a R,
	) -> Difference<&'a Self::Item>
	where
		Self::Item: PartialOrd + Measure,
	{
		let left = match invert_bound(other.start()) {
			Some(inverted_other_start) => {
				if !is_range_empty(self.start(), inverted_other_start) {
					Some(AnyRange {
						start: self.start(),
						end: inverted_other_start,
					})
				} else {
					None
				}
			}
			None => None,
		};

		let right = match invert_bound(other.end()) {
			Some(inverted_other_end) => {
				if !is_range_empty(inverted_other_end, self.end()) {
					Some(AnyRange {
						start: inverted_other_end,
						end: self.end(),
					})
				} else {
					None
				}
			}
			None => None,
		};

		match (left, right) {
			(Some(left), None) => Difference::Before(
				left,
				Directed::End(left.end) >= Directed::Start(other.start()),
			),
			(None, Some(right)) => Difference::After(
				right,
				Directed::Start(right.start) <= Directed::End(other.end()),
			),
			(Some(left), Some(right)) => Difference::Split(left, right),
			(None, None) => Difference::Empty,
		}
	}

	fn product<'a, R: AsRange<Item = Self::Item>>(&'a self, other: &'a R) -> Product<&'a Self::Item>
	where
		Self::Item: PartialOrd + Measure,
	{
		let before = match crop_right(self, other.start()) {
			Some(self_before) => Some(ProductArg::Subject(self_before)),
			None => crop_right(other, self.start()).map(ProductArg::Object),
		};

		let intersection = self.intersected_with(other);
		let intersection = if is_range_empty(intersection.start, intersection.end) {
			None
		} else {
			Some(intersection)
		};

		let after = match crop_left(self, other.end()) {
			Some(self_after) => Some(ProductArg::Subject(self_after)),
			None => crop_left(other, self.end()).map(ProductArg::Object),
		};

		Product {
			before,
			intersection,
			after,
		}
	}
}

fn crop_left<'a, R: AsRange>(
	range: &'a R,
	other_end: Bound<&'a R::Item>,
) -> Option<AnyRange<&'a R::Item>> {
	match invert_bound(other_end) {
		Some(inverted_other_end) => {
			let max_start = max_bound(range.start(), inverted_other_end, true);
			if !is_range_empty(max_start, range.end()) {
				Some(AnyRange {
					start: inverted_other_end,
					end: range.end(),
				})
			} else {
				None
			}
		}
		None => None,
	}
}

fn crop_right<'a, R: AsRange>(
	range: &'a R,
	other_start: Bound<&'a R::Item>,
) -> Option<AnyRange<&'a R::Item>> {
	match invert_bound(other_start) {
		Some(inverted_other_start) => {
			let min_end = min_bound(range.end(), inverted_other_start, false);
			if !is_range_empty(range.start(), min_end) {
				Some(AnyRange {
					start: range.start(),
					end: min_end,
				})
			} else {
				None
			}
		}
		None => None,
	}
}

/// Part of the result of a `product` operation.
pub enum ProductArg<T> {
	/// A part of the subject, `self`.
	Subject(AnyRange<T>),

	/// A part of the object, `other`.
	Object(AnyRange<T>),
}

impl<'a, T: Clone> ProductArg<&'a T> {
	pub fn cloned(&self) -> ProductArg<T> {
		match self {
			ProductArg::Subject(range) => ProductArg::Subject(range.cloned()),
			ProductArg::Object(range) => ProductArg::Object(range.cloned()),
		}
	}
}

/// Result of a `product` operation.
pub struct Product<T> {
	/// What is left of `self` and `other` before their intersection.
	pub before: Option<ProductArg<T>>,

	/// The intersection of `self` and `other`, if not empty.
	pub intersection: Option<AnyRange<T>>,

	/// What is left of `self` and `other` after their intersection.
	pub after: Option<ProductArg<T>>,
}

impl<'a, T: Clone> Product<&'a T> {
	pub fn cloned(&self) -> Product<T> {
		Product {
			before: self.before.as_ref().map(|r| r.cloned()),
			intersection: self.intersection.as_ref().map(|r| r.cloned()),
			after: self.after.as_ref().map(|r| r.cloned()),
		}
	}
}

pub enum RelativePosition {
	Before,
	After,
}

/// Result of a `without` operation.
pub enum Difference<T> {
	/// The end of the range may intersects `other`. The boolean is set to true if it does.
	Before(AnyRange<T>, bool),

	/// The begining of the range may intersects `other`. The boolean is set to true if it does.
	After(AnyRange<T>, bool),

	/// The `other` range if fully included.
	Split(AnyRange<T>, AnyRange<T>),

	/// The range is fully included in `other`.
	Empty,
}

macro_rules! singleton_range {
	($ty:ident) => {
		impl AsRange for $ty {
			type Item = Self;

			fn start(&self) -> Bound<&Self::Item> {
				Bound::Included(self)
			}

			fn end(&self) -> Bound<&Self::Item> {
				Bound::Included(self)
			}
		}
	};
}

singleton_range!(u8);
singleton_range!(i8);
singleton_range!(u16);
singleton_range!(i16);
singleton_range!(u32);
singleton_range!(i32);
singleton_range!(u64);
singleton_range!(i64);
// singleton_range!(u128);
// singleton_range!(i128);
singleton_range!(usize);
// singleton_range!(isize);
singleton_range!(f32);
singleton_range!(f64);
singleton_range!(char);

macro_rules! standard_range {
	($ty:path) => {
		impl<T: Measure + PartialOrd> AsRange for $ty {
			type Item = T;

			fn start(&self) -> Bound<&Self::Item> {
				self.start_bound()
			}

			fn end(&self) -> Bound<&Self::Item> {
				self.end_bound()
			}
		}
	};
}

standard_range!(std::ops::Range<T>);
standard_range!(std::ops::RangeInclusive<T>);
standard_range!(std::ops::RangeFrom<T>);
standard_range!(std::ops::RangeTo<T>);
standard_range!(std::ops::RangeToInclusive<T>);
standard_range!(AnyRange<T>);
standard_range!(RangeFromExcluded<T>);
standard_range!(RangeFromExcludedTo<T>);
standard_range!(RangeFromExcludedToIncluded<T>);

#[inline(always)]
fn is_range_empty<T, U>(start: Bound<&T>, end: Bound<&U>) -> bool
where
	T: PartialOrd<U> + Measure<U>,
{
	Directed::Start(start) > Directed::End(end)
}

#[cfg(test)]
mod tests {
	use super::*;
	use std::cmp::Ordering;

	macro_rules! make_bound {
		([= $v:literal ..]) => {
			Directed::Start(Bound::Included(&$v))
		};
		([$v:literal ..]) => {
			Directed::Start(Bound::Excluded(&$v))
		};
		([~ ..]) => {
			Directed::Start(Bound::Unbounded)
		};
		([..= $v:literal]) => {
			Directed::End(Bound::Included(&$v))
		};
		([.. $v:literal]) => {
			Directed::End(Bound::Excluded(&$v))
		};
		([.. ~]) => {
			Directed::End(Bound::Unbounded)
		};
	}

	macro_rules! test_bound_cmp {
		(@assert $ty:ty, $a:tt, $b:tt, $expected:ident) => {
			assert_eq!(<Directed<Bound<&$ty>> as PartialOrd>::partial_cmp(&make_bound!($a), &make_bound!($b)), Some(Ordering::$expected));
		};
		($ty:ty, $a:tt < $b:tt) => {
			test_bound_cmp!(@assert $ty, $a, $b, Less)
		};
		($ty:ty, $a:tt == $b:tt) => {
			test_bound_cmp!(@assert $ty, $a, $b, Equal)
		};
		($ty:ty, $a:tt > $b:tt) => {
			test_bound_cmp!(@assert $ty, $a, $b, Greater)
		}
	}

	#[test]
	fn integer_bound_partial_less() {
		test_bound_cmp!(i32, [=0..] < [=1..]);
		test_bound_cmp!(i32, [=0..] < [0..]);
		test_bound_cmp!(i32, [=0..] < [..=1]);
		test_bound_cmp!(i32, [=0..] < [..2]);
		test_bound_cmp!(i32, [=0..] < [..~]);

		test_bound_cmp!(i32, [0..] < [=2..]);
		test_bound_cmp!(i32, [0..] < [1..]);
		test_bound_cmp!(i32, [0..] < [..=2]);
		test_bound_cmp!(i32, [0..] < [..3]);
		test_bound_cmp!(i32, [0..] < [..~]);

		test_bound_cmp!(i32, [~..] < [=0..]);
		test_bound_cmp!(i32, [~..] < [..=0]);
		test_bound_cmp!(i32, [~..] < [..0]);
		test_bound_cmp!(i32, [~..] < [..~]);

		test_bound_cmp!(i32, [..=0] < [=1..]);
		test_bound_cmp!(i32, [..=0] < [0..]);
		test_bound_cmp!(i32, [..=0] < [..=1]);
		test_bound_cmp!(i32, [..=0] < [..2]);
		test_bound_cmp!(i32, [..=0] < [..~]);

		test_bound_cmp!(i32, [..1] < [=1..]);
		test_bound_cmp!(i32, [..1] < [0..]);
		test_bound_cmp!(i32, [..1] < [..=1]);
		test_bound_cmp!(i32, [..1] < [..2]);
		test_bound_cmp!(i32, [..0] < [..~]);
	}

	#[test]
	fn integer_bound_partial_eq() {
		test_bound_cmp!(i32, [=0..] == [=0..]);
		test_bound_cmp!(i32, [=1..] == [0..]);
		test_bound_cmp!(i32, [=0..] == [..=0]);
		test_bound_cmp!(i32, [=0..] == [..1]);

		test_bound_cmp!(i32, [0..] == [=1..]);
		test_bound_cmp!(i32, [0..] == [0..]);
		test_bound_cmp!(i32, [0..] == [..=1]);
		test_bound_cmp!(i32, [0..] == [..2]);

		test_bound_cmp!(i32, [~..] == [~..]);

		test_bound_cmp!(i32, [..=0] == [=0..]);
		test_bound_cmp!(i32, [..=1] == [0..]);
		test_bound_cmp!(i32, [..=0] == [..=0]);
		test_bound_cmp!(i32, [..=0] == [..1]);

		test_bound_cmp!(i32, [..1] == [=0..]);
		test_bound_cmp!(i32, [..2] == [0..]);
		test_bound_cmp!(i32, [..1] == [..=0]);
		test_bound_cmp!(i32, [..0] == [..0]);

		test_bound_cmp!(i32, [..~] == [..~]);
	}

	#[test]
	fn integer_bound_partial_greater() {
		test_bound_cmp!(i32, [=1..] > [=0..]);
		test_bound_cmp!(i32, [0..] > [=0..]);
		test_bound_cmp!(i32, [..=1] > [=0..]);
		test_bound_cmp!(i32, [..2] > [=0..]);
		test_bound_cmp!(i32, [..~] > [=0..]);

		test_bound_cmp!(i32, [=2..] > [0..]);
		test_bound_cmp!(i32, [1..] > [0..]);
		test_bound_cmp!(i32, [..=2] > [0..]);
		test_bound_cmp!(i32, [..3] > [0..]);
		test_bound_cmp!(i32, [..~] > [0..]);

		test_bound_cmp!(i32, [=0..] > [~..]);
		test_bound_cmp!(i32, [..=0] > [~..]);
		test_bound_cmp!(i32, [..0] > [~..]);
		test_bound_cmp!(i32, [..~] > [~..]);

		test_bound_cmp!(i32, [=1..] > [..=0]);
		test_bound_cmp!(i32, [0..] > [..=0]);
		test_bound_cmp!(i32, [..=1] > [..=0]);
		test_bound_cmp!(i32, [..2] > [..=0]);
		test_bound_cmp!(i32, [..~] > [..=0]);

		test_bound_cmp!(i32, [=1..] > [..1]);
		test_bound_cmp!(i32, [0..] > [..1]);
		test_bound_cmp!(i32, [..=1] > [..1]);
		test_bound_cmp!(i32, [..2] > [..1]);
		test_bound_cmp!(i32, [..~] > [..0]);
	}

	#[test]
	fn float_bound_partial_less() {
		test_bound_cmp!(f32, [=0.0..] < [=1.0..]);
		test_bound_cmp!(f32, [=0.0..] < [0.0..]);
		test_bound_cmp!(f32, [=0.0..] < [..=1.0]);
		test_bound_cmp!(f32, [=0.0..] < [..2.0]);
		test_bound_cmp!(f32, [=0.0..] < [..~]);

		test_bound_cmp!(f32, [0.0..] < [=2.0..]);
		test_bound_cmp!(f32, [0.0..] < [1.0..]);
		test_bound_cmp!(f32, [0.0..] < [..1.0]); // different from the int behavior
		test_bound_cmp!(f32, [0.0..] < [..=2.0]);
		test_bound_cmp!(f32, [0.0..] < [..3.0]);
		test_bound_cmp!(f32, [0.0..] < [..~]);

		test_bound_cmp!(f32, [~..] < [=0.0..]);
		test_bound_cmp!(f32, [~..] < [..=0.0]);
		test_bound_cmp!(f32, [~..] < [..0.0]);
		test_bound_cmp!(f32, [~..] < [..~]);

		test_bound_cmp!(f32, [..=0.0] < [=1.0..]);
		test_bound_cmp!(f32, [..=0.0] < [0.0..]);
		test_bound_cmp!(f32, [..=0.0] < [..=1.0]);
		test_bound_cmp!(f32, [..=0.0] < [..2.0]);
		test_bound_cmp!(f32, [..=0.0] < [..~]);

		test_bound_cmp!(f32, [..1.0] < [=1.0..]);
		test_bound_cmp!(f32, [..1.0] < [1.0..]);
		test_bound_cmp!(f32, [..1.0] < [..=1.0]);
		test_bound_cmp!(f32, [..1.0] < [..2.0]);
		test_bound_cmp!(f32, [..0.0] < [..~]);
	}

	#[test]
	fn float_bound_partial_eq() {
		test_bound_cmp!(f32, [=0.0..] == [=0.0..]);
		test_bound_cmp!(f32, [=1.0..] > [0.0..]); // different from the int behavior
		test_bound_cmp!(f32, [=0.0..] == [..=0.0]);
		test_bound_cmp!(f32, [=0.0..] < [..1.0]); // different from the int behavior

		test_bound_cmp!(f32, [0.0..] < [=1.0..]); // different from the int behavior
		test_bound_cmp!(f32, [0.0..] == [0.0..]);
		test_bound_cmp!(f32, [0.0..] < [..=1.0]); // different from the int behavior
		test_bound_cmp!(f32, [0.0..] < [..2.0]); // different from the int behavior

		test_bound_cmp!(f32, [~..] == [~..]);

		test_bound_cmp!(f32, [..=0.0] == [=0.0..]);
		test_bound_cmp!(f32, [..=1.0] > [0.0..]); // different from the int behavior
		test_bound_cmp!(f32, [..=0.0] == [..=0.0]);
		test_bound_cmp!(f32, [..=0.0] < [..1.0]); // different from the int behavior

		test_bound_cmp!(f32, [..1.0] > [=0.0..]); // different from the int behavior
		test_bound_cmp!(f32, [..2.0] > [0.0..]); // different from the int behavior
		test_bound_cmp!(f32, [..1.0] > [..=0.0]); // different from the int behavior
		test_bound_cmp!(f32, [..0.0] == [..0.0]);

		test_bound_cmp!(f32, [..~] == [..~]);
	}

	#[test]
	fn float_bound_partial_greater() {
		test_bound_cmp!(f32, [=1.0..] > [=0.0..]);
		test_bound_cmp!(f32, [0.0..] > [=0.0..]);
		test_bound_cmp!(f32, [..=1.0] > [=0.0..]);
		test_bound_cmp!(f32, [..2.0] > [=0.0..]);
		test_bound_cmp!(f32, [..~] > [=0.0..]);

		test_bound_cmp!(f32, [=2.0..] > [0.0..]);
		test_bound_cmp!(f32, [1.0..] > [0.0..]);
		test_bound_cmp!(f32, [..1.0] > [0.0..]); // different from the int behavior
		test_bound_cmp!(f32, [..=2.0] > [0.0..]);
		test_bound_cmp!(f32, [..3.0] > [0.0..]);
		test_bound_cmp!(f32, [..~] > [0.0..]);

		test_bound_cmp!(f32, [=0.0..] > [~..]);
		test_bound_cmp!(f32, [..=0.0] > [~..]);
		test_bound_cmp!(f32, [..0.0] > [~..]);
		test_bound_cmp!(f32, [..~] > [~..]);

		test_bound_cmp!(f32, [=1.0..] > [..=0.0]);
		test_bound_cmp!(f32, [0.0..] > [..=0.0]);
		test_bound_cmp!(f32, [..=1.0] > [..=0.0]);
		test_bound_cmp!(f32, [..2.0] > [..=0.0]);
		test_bound_cmp!(f32, [..~] > [..=0.0]);

		test_bound_cmp!(f32, [=1.0..] > [..1.0]);
		test_bound_cmp!(f32, [1.0..] > [..1.0]);
		test_bound_cmp!(f32, [..=1.0] > [..1.0]);
		test_bound_cmp!(f32, [..2.0] > [..1.0]);
		test_bound_cmp!(f32, [..~] > [..0.0]);
	}

	#[test]
	fn int_intersection() {
		assert!((0..10).intersects(&(5..100)));
	}

	// Intersecting ranges are connected.
	#[test]
	fn int_connected_intersection() {
		assert!((0..10).connected_to(&(5..100)));
	}

	#[test]
	fn int_connected() {
		assert!((0..10).connected_to(&(10..20)));
		assert!((10..20).connected_to(&(0..10)));
		assert!((0..=10).connected_to(&(RangeFromExcludedTo::new(10, 20))));
	}

	#[test]
	fn int_disconnected() {
		assert!(!(0..10).connected_to(&(11..20)));
		assert!(!(11..20).connected_to(&(0..10)));
		assert!(!(0..10).connected_to(&(RangeFromExcludedTo::new(10, 20))));
	}

	#[test]
	fn float_connected() {
		assert!((0.0..10.0).connected_to(&(10.0..20.0)));
		assert!((0.0..=10.0).connected_to(&(RangeFromExcludedTo::new(10.0, 20.0))));
	}

	#[test]
	fn float_disconnected() {
		assert!(!(0.0..10.0).connected_to(&(RangeFromExcludedTo::new(10.0, 20.0))));
		assert!(!(..10.0).connected_to(&(RangeFromExcludedTo::new(10.0, 20.0))));
		assert!(!(0.0..10.0).connected_to(&(RangeFromExcluded::new(10.0))));
		assert!(!(..10.0).connected_to(&(RangeFromExcluded::new(10.0))));
	}
}
