use std::{
	fmt::Debug,
	ops::{
		RangeBounds,
		Bound
	},
	cmp::{
		Ord,
		PartialOrd,
		Ordering
	}
};

// `a...b`
pub struct RangeFromExcludedTo<T> {
	pub start: T,
	pub end: T
}

impl<T> RangeFromExcludedTo<T> {
	pub const fn new(start: T, end: T) -> RangeFromExcludedTo<T> {
		RangeFromExcludedTo {
			start, end
		}
	}
}

impl<T> RangeBounds<T> for RangeFromExcludedTo<T> {
	fn start_bound(&self) -> Bound<&T> {
		Bound::Excluded(&self.start)
	}

	fn end_bound(&self) -> Bound<&T> {
		Bound::Excluded(&self.end)
	}
}

// `a...=b`
pub struct RangeFromExcludedToIncluded<T> {
	pub start: T,
	pub end: T
}

impl<T> RangeFromExcludedToIncluded<T> {
	pub const fn new(start: T, end: T) -> RangeFromExcludedToIncluded<T> {
		RangeFromExcludedToIncluded {
			start, end
		}
	}
}

impl<T> RangeBounds<T> for RangeFromExcludedToIncluded<T> {
	fn start_bound(&self) -> Bound<&T> {
		Bound::Excluded(&self.start)
	}

	fn end_bound(&self) -> Bound<&T> {
		Bound::Included(&self.end)
	}
}

// `a...`
pub struct RangeFromExcluded<T> {
	pub start: T
}

impl<T> RangeFromExcluded<T> {
	pub const fn new(start: T) -> RangeFromExcluded<T> {
		RangeFromExcluded {
			start
		}
	}
}

impl<T> RangeBounds<T> for RangeFromExcluded<T> {
	fn start_bound(&self) -> Bound<&T> {
		Bound::Excluded(&self.start)
	}

	fn end_bound(&self) -> Bound<&T> {
		Bound::Unbounded
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum BoundDirection {
	Start,
	End
}

pub trait BoundPartialOrd<T = Self> {
	fn bound_partial_cmp(a: (Bound<&Self>, BoundDirection), b: (Bound<&T>, BoundDirection)) -> Option<Ordering>;
}

pub trait IncludedBound: Sized {
	fn included_bound(bound: Bound<&Self>, direction: BoundDirection) -> Option<Self>;
}

impl<U: IncludedBound, T: IncludedBound + PartialOrd<U>> BoundPartialOrd<U> for T {
	fn bound_partial_cmp((a, a_direction): (Bound<&T>, BoundDirection), (b, b_direction): (Bound<&U>, BoundDirection)) -> Option<Ordering> {
		match (T::included_bound(a, a_direction), U::included_bound(b, b_direction)) {
			(Some(a), Some(b)) => a.partial_cmp(&b),
			(None, None) => Some(a_direction.cmp(&b_direction)),
			(Some(_), None) => match a_direction.cmp(&b_direction) {
				Ordering::Equal if a_direction == BoundDirection::Start => Some(Ordering::Greater),
				Ordering::Equal => Some(Ordering::Less),
				ordering => Some(ordering)
			},
			(None, Some(_)) => match a_direction.cmp(&b_direction) {
				Ordering::Equal if a_direction == BoundDirection::Start => Some(Ordering::Less),
				Ordering::Equal => Some(Ordering::Greater),
				ordering => Some(ordering)
			}
		}
	}
}

macro_rules! int_included_bound {
	($ty:ident) => {
		impl IncludedBound for $ty {
			fn included_bound(bound: Bound<&Self>, direction: BoundDirection) -> Option<Self> {
				match bound {
					Bound::Included(a) => Some(*a),
					Bound::Excluded(a) => {
						match direction {
							BoundDirection::Start if *a == std::$ty::MAX => None,
							BoundDirection::Start => Some(a+1),
							BoundDirection::End if *a == std::$ty::MIN => None,
							BoundDirection::End => Some(a-1)
						}
					},
					Bound::Unbounded => {
						match direction {
							BoundDirection::Start => Some(std::$ty::MIN),
							BoundDirection::End => Some(std::$ty::MAX)
						}
					}
				}
			}
		}
	}
}

int_included_bound!(u8);
int_included_bound!(i8);
int_included_bound!(u16);
int_included_bound!(i16);
int_included_bound!(u32);
int_included_bound!(i32);
int_included_bound!(u64);
int_included_bound!(i64);
int_included_bound!(u128);
int_included_bound!(i128);
int_included_bound!(usize);
int_included_bound!(isize);

macro_rules! float_bound_partial_ord {
	($ty:ident) => {
		impl BoundPartialOrd for $ty {
			fn bound_partial_cmp((a, a_direction): (Bound<&Self>, BoundDirection), (b, b_direction): (Bound<&Self>, BoundDirection)) -> Option<Ordering> {
				match (a, b) {
					(Bound::Included(a), Bound::Included(b)) => a.partial_cmp(b),
					(Bound::Included(a), Bound::Excluded(b)) => match a.partial_cmp(b) {
						Some(Ordering::Less) => Some(Ordering::Less),
						Some(Ordering::Equal) => if b_direction == BoundDirection::Start {
							Some(Ordering::Less)
						} else {
							Some(Ordering::Greater)
						},
						Some(Ordering::Greater) => Some(Ordering::Greater),
						None => None
					},
					(Bound::Included(_), Bound::Unbounded) => if b_direction == BoundDirection::Start {
						Some(Ordering::Greater)
					} else {
						Some(Ordering::Less)
					},
					(Bound::Excluded(a), Bound::Included(b)) => match a.partial_cmp(b) {
						Some(Ordering::Less) => Some(Ordering::Less),
						Some(Ordering::Equal) => if a_direction == BoundDirection::Start {
							Some(Ordering::Greater)
						} else {
							Some(Ordering::Less)
						},
						Some(Ordering::Greater) => Some(Ordering::Greater),
						None => None
					},
					(Bound::Excluded(a), Bound::Excluded(b)) => match a.partial_cmp(b) {
						Some(Ordering::Equal) => Some(b_direction.cmp(&a_direction)),
						Some(ordering) => Some(ordering),
						None => None
					},
					(Bound::Excluded(_), Bound::Unbounded) => if b_direction == BoundDirection::Start {
						Some(Ordering::Greater)
					} else {
						Some(Ordering::Less)
					},
					(Bound::Unbounded, Bound::Unbounded) => Some(a_direction.cmp(&b_direction)),
					(Bound::Unbounded, _) => if a_direction == BoundDirection::Start {
						Some(Ordering::Less)
					} else {
						Some(Ordering::Greater)
					}
				}
			}
		}
	}
}

float_bound_partial_ord!(f32);
float_bound_partial_ord!(f64);

fn min_start_bound<'a, T: BoundPartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>) -> Bound<&'a T> {
	match T::bound_partial_cmp((a, BoundDirection::Start), (b, BoundDirection::Start)) {
		Some(Ordering::Less) => a,
		_ => b
	}
}

fn max_end_bound<'a, T: BoundPartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>) -> Bound<&'a T> {
	match T::bound_partial_cmp((a, BoundDirection::End), (b, BoundDirection::End)) {
		Some(Ordering::Greater) => a,
		_ => b
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Range<T> {
	start: Bound<T>,
	end: Bound<T>
}

impl<T> Range<T> {
	pub fn add<S>(&mut self, other: &S) where T: Clone + BoundPartialOrd, S: RangeBounds<T> {
		self.start = min_start_bound(self.start_bound(), other.start_bound()).cloned();
		self.end = max_end_bound(self.end_bound(), other.end_bound()).cloned();
	}
}

impl<T: Clone> Range<&T> {
	pub fn cloned(self) -> Range<T> {
		Range {
			start: self.start.cloned(),
			end: self.end.cloned()
		}
	}
}

impl<T> RangeBounds<T> for Range<T> {
	fn start_bound(&self) -> Bound<&T> {
		match &self.start {
			Bound::Included(v) => Bound::Included(v),
			Bound::Excluded(v) => Bound::Excluded(v),
			Bound::Unbounded => Bound::Unbounded
		}
	}

	fn end_bound(&self) -> Bound<&T> {
		match &self.start {
			Bound::Included(v) => Bound::Included(v),
			Bound::Excluded(v) => Bound::Excluded(v),
			Bound::Unbounded => Bound::Unbounded
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RangeOrdering {
	Before,
	ConnectedBefore,
	Intersecting,
	ConnectedAfter,
	After
}

impl RangeOrdering {
	pub fn is_before(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::Before => true,
			RangeOrdering::ConnectedBefore => !disconnected,
			_ => false
		}
	}

	pub fn is_after(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::After => true,
			RangeOrdering::ConnectedAfter => !disconnected,
			_ => false
		}
	}

	pub fn matches(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::ConnectedBefore => !disconnected,
			RangeOrdering::Intersecting => true,
			RangeOrdering::ConnectedAfter => !disconnected,
			_ => false
		}
	}
}

pub trait RangeOrd<T = Self>: Sized {
	fn range_cmp(a: &Range<Self>, b: &T) -> RangeOrdering;
}

impl<U, T: BoundPartialOrd> RangeOrd<Range<U>> for T {
	fn range_cmp(a: &Range<Self>, b: &Range<U>) -> RangeOrdering {
		panic!("TODO")
	}
}

impl<U, T: BoundPartialOrd<U>> PartialEq<std::ops::Range<U>> for Range<T> {
	fn eq(&self, other: &std::ops::Range<U>) -> bool {
		T::bound_partial_cmp((self.start_bound(), BoundDirection::Start), (other.start_bound(), BoundDirection::Start)) == Some(Ordering::Equal) &&
		T::bound_partial_cmp((self.end_bound(), BoundDirection::End), (other.end_bound(), BoundDirection::End)) == Some(Ordering::Equal)
	}
}

impl<U, T: BoundPartialOrd<U>> PartialOrd<std::ops::Range<U>> for Range<T> {
	fn partial_cmp(&self, other: &std::ops::Range<U>) -> Option<Ordering> {
		//T::bound_partial_cmp((self.start_bound(), BoundDirection::Start), (other.start_bound(), BoundDirection::Start))
		None
	}
}

impl<T: Clone> From<T> for Range<T> {
	fn from(t: T) -> Range<T> {
		Range {
			start: Bound::Included(t.clone()),
			end: Bound::Included(t)
		}
	}
}

impl<T> From<std::ops::Range<T>> for Range<T> {
	fn from(range: std::ops::Range<T>) -> Range<T> {
		Range {
			start: Bound::Included(range.start),
			end: Bound::Excluded(range.end)
		}
	}
}

pub trait RangeExt<T> {
	fn is_empty(&self) -> bool where T: BoundPartialOrd;
	
	fn intersects<U, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U>;

	fn connected_to<U, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U>;

	// Output ranges are never empty.
	fn without<'a, S>(&'a self, other: &'a S) -> (Option<Range<&'a T>>, Option<Range<&'a T>>) where T: BoundPartialOrd, S: RangeBounds<T>;
}

#[inline(always)]
fn invert_bound<T>(bound: Bound<T>) -> Option<Bound<T>> {
	match bound {
		Bound::Unbounded => None,
		Bound::Included(t) => Some(Bound::Excluded(t)),
		Bound::Excluded(t) => Some(Bound::Included(t))
	}
}

#[inline(always)]
fn is_range_empty<T, U>(start: Bound<&T>, end: Bound<&U>) -> bool where T: BoundPartialOrd<U> {
	T::bound_partial_cmp((start, BoundDirection::Start), (end, BoundDirection::End)) == Some(Ordering::Greater)
}

#[inline(always)]
fn are_conneted_bounds<T, U>(end: Bound<&T>, start: Bound<&U>) -> bool where T: BoundPartialOrd<U> {
	match (invert_bound(end), invert_bound(start)) {
		(Some(start), Some(end)) => is_range_empty(start, end),
		_ => true
	}
}

impl<T, R: RangeBounds<T>> RangeExt<T> for R {
	fn is_empty(&self) -> bool where T: BoundPartialOrd {
		is_range_empty(self.start_bound(), self.end_bound())
	}

	fn intersects<U, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U> {
		!is_range_empty(self.start_bound(), other.end_bound()) || !is_range_empty(other.start_bound(), self.end_bound())
	}

	fn connected_to<U, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U> {
		are_conneted_bounds(self.end_bound(), other.start_bound()) && are_conneted_bounds(other.end_bound(), self.start_bound())
	}

	fn without<'a, S>(&'a self, other: &'a S) -> (Option<Range<&'a T>>, Option<Range<&'a T>>) where T: BoundPartialOrd, S: RangeBounds<T> {
		let left = match invert_bound(other.start_bound()) {
			Some(inverted_other_start) => if !is_range_empty(self.start_bound(), inverted_other_start) {
				Some(Range {
					start: self.start_bound(),
					end: inverted_other_start
				})
			} else {
				None
			},
			None => None
		};

		let right = match invert_bound(other.end_bound()) {
			Some(inverted_other_end) => if !is_range_empty(inverted_other_end, self.end_bound()) {
				Some(Range {
					start: inverted_other_end,
					end: self.start_bound()
				})
			} else {
				None
			},
			None => None
		};

		(left, right)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	macro_rules! make_bound {
		([= $v:literal ..]) => { (Bound::Included(&$v), BoundDirection::Start) };
		([$v:literal ..]) => { (Bound::Excluded(&$v), BoundDirection::Start) };
		([~ ..]) => { (Bound::Unbounded, BoundDirection::Start) };
		([..= $v:literal]) => { (Bound::Included(&$v), BoundDirection::End) };
		([.. $v:literal]) => { (Bound::Excluded(&$v), BoundDirection::End) };
		([.. ~]) => { (Bound::Unbounded, BoundDirection::End) }
	}

	macro_rules! test_bound_cmp {
		(@assert $ty:ty, $a:tt, $b:tt, $expected:ident) => {
			assert_eq!(<$ty as BoundPartialOrd>::bound_partial_cmp(make_bound!($a), make_bound!($b)), Some(Ordering::$expected));
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

// impl<T> From<Range<T>> for ExclusiveRange<T> {
// 	fn from(range: Range<T>) -> ExclusiveRange<T> {
// 		ExclusiveRange {
// 			start: range.start,
// 			end: range.end
// 		}
// 	}
// }

// impl<T: Ord> Ord for ExclusiveRange<T> {
// 	fn cmp(&self, other: &ExclusiveRange<T>) -> Ordering {
// 		// It is very important that the end bound is used for comparison here.
// 		// Because for a brief moment in the range insertion algorithm, exclusives ranges may not be ordered by `start`.
// 		self.end.cmp(&other.end)
// 	}
// }

// impl<T: Ord> PartialOrd for ExclusiveRange<T> {
// 	fn partial_cmp(&self, other: &ExclusiveRange<T>) -> Option<Ordering> {
// 		Some(self.cmp(other))
// 	}
// }

// impl<T: PartialEq> PartialEq<Range<T>> for ExclusiveRange<T> {
// 	fn eq(&self, other: &Range<T>) -> bool {
// 		self.start.eq(&other.start) && self.end.eq(&other.end)
// 	}
// }

// impl<T: PartialEq> PartialEq for ExclusiveRange<T> {
// 	fn eq(&self, other: &ExclusiveRange<T>) -> bool {
// 		self.start.eq(&other.start) && self.end.eq(&other.end)
// 	}
// }

// impl<T: Eq> Eq for ExclusiveRange<T> { }
