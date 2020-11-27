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

#[derive(Debug, Clone)]
pub struct AnyRange<T> {
	start: Bound<T>,
	end: Bound<T>
}

impl<T> AnyRange<T> {
	pub fn from<R: AsRange<Item=T>>(range: R) -> AnyRange<T> where T: Clone {
		AnyRange {
			start: range.start().cloned(),
			end: range.end().cloned()
		}
	}

	pub fn add<S>(&mut self, other: &S) where T: Clone + Measure + PartialOrd, S: RangeBounds<T> {
		self.start = max_bound(self.start_bound(), other.start_bound(), true).cloned();
		self.end = max_bound(self.end_bound(), other.end_bound(), false).cloned();
	}
}

impl<T: Clone> AnyRange<&T> {
	pub fn cloned(self) -> AnyRange<T> {
		AnyRange {
			start: self.start.cloned(),
			end: self.end.cloned()
		}
	}
}

impl<T> RangeBounds<T> for AnyRange<T> {
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

/// Types that can be interpreted as ranges.
pub trait AsRange: Sized {
	/// Type of the elements of the range.
	type Item : Measure + PartialOrd;

	/// Start bound of the range.
	fn start(&self) -> Bound<&Self::Item>;

	/// End bound of the range.
	fn end(&self) -> Bound<&Self::Item>;

	fn is_empty(&self) -> bool {
		is_range_empty(self.start(), self.end())
	}

	fn intersects<R: AsRange>(&self, other: &R) -> bool where Self::Item: PartialOrd<R::Item> + Measure<R::Item> {
		match self.range_partial_cmp(other) {
			Some(RangeOrdering::Intersecting(_, _)) => true,
			_ => false
		}
	}

	fn connected_to<R: AsRange>(&self, other: &R) -> bool where Self::Item: PartialOrd<R::Item> + Measure<R::Item> {
		match self.range_partial_cmp(other) {
			Some(RangeOrdering::Intersecting(_, _)) => true,
			Some(RangeOrdering::Before(connected)) => connected,
			Some(RangeOrdering::After(connected)) => connected,
			_ => false
		}
	}

	fn without<'a, R: AsRange<Item=Self::Item>>(&'a self, other: &'a R) -> (Option<AnyRange<&'a Self::Item>>, Option<AnyRange<&'a Self::Item>>) {
		let left = match invert_bound(other.start()) {
			Some(inverted_other_start) => if !is_range_empty(self.start(), inverted_other_start) {
				Some(AnyRange {
					start: self.start(),
					end: inverted_other_start
				})
			} else {
				None
			},
			None => None
		};

		let right = match invert_bound(other.end()) {
			Some(inverted_other_end) => if !is_range_empty(inverted_other_end, self.end()) {
				Some(AnyRange {
					start: inverted_other_end,
					end: self.start()
				})
			} else {
				None
			},
			None => None
		};

		(left, right)
	}
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
	}
}

singleton_range!(u8);
singleton_range!(i8);
singleton_range!(u16);
singleton_range!(i16);
singleton_range!(u32);
singleton_range!(i32);
singleton_range!(u64);
singleton_range!(i64);
singleton_range!(u128);
singleton_range!(i128);
singleton_range!(usize);
singleton_range!(isize);

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
	}
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

/// Types that can be interpreted as range bounds.
pub trait AsBound {
	type Item;

	fn bound(&self) -> Bound<&Self::Item>;
}

impl AsBound for u8 {
	type Item = u8;

	fn bound(&self) -> Bound<&u8> {
		Bound::Included(self)
	}
}

impl<'a, T> AsBound for Bound<&'a T> {
	type Item = T;

	fn bound(&self) -> Bound<&T> {
		*self
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Distance {
	/// Both elements are equals.
	Zero,

	/// Both elements are different, but there is no element between them.
	One,

	/// Both elements are different, and there is only one other elements between them.
	Two, 

	/// Both elements are different, and there are multiples elements between them.
	More
}

/// Distance between singletons.
pub trait Measure<U = Self> {
	fn distance(&self, other: &U) -> Distance;
}

macro_rules! impl_measure {
	(@both $ty1:ty, $ty2:ty, $cast:ty) => {
		impl_measure!($ty1, $ty2, $cast);
		impl_measure!($ty2, $ty1, $cast);
	};
	($ty1:ty, $ty2:ty, $cast:ty) => {
		impl Measure<$ty2> for $ty1 {
			fn distance(&self, other: &$ty2) -> Distance {
				let d = if (*self as $cast) > (*other as $cast) {
					*self as $cast
				} else {
					*other as $cast
				};

				match d {
					0 => Distance::Zero,
					1 => Distance::One,
					2 => Distance::Two,
					_ => Distance::More
				}
			}
		}
	}
}

impl_measure!(u8, u8, u8);
impl_measure!(@both u8, u16, u16);
impl_measure!(@both u8, u32, u32);
impl_measure!(@both u8, u64, u64);
impl_measure!(@both u8, u128, u128);
impl_measure!(@both u8, usize, usize);
impl_measure!(@both u8, i8, i16);
impl_measure!(@both u8, i16, i16);
impl_measure!(@both u8, i32, i32);
impl_measure!(@both u8, i64, i64);
impl_measure!(@both u8, isize, isize);
impl_measure!(u16, u16, u16);
impl_measure!(@both u16, u32, u32);
impl_measure!(@both u16, u64, u64);
impl_measure!(@both u16, u128, u128);
impl_measure!(@both u16, usize, usize);
impl_measure!(@both u16, i8, i32);
impl_measure!(@both u16, i16, i32);
impl_measure!(@both u16, i32, i32);
impl_measure!(@both u16, i64, i64);
impl_measure!(@both u16, i128, i128);
impl_measure!(@both u16, isize, isize);
impl_measure!(u32, u32, u32);
impl_measure!(@both u32, u64, u64);
impl_measure!(@both u32, u128, u128);
impl_measure!(@both u32, usize, usize);
impl_measure!(@both u32, i8, i16);
impl_measure!(@both u32, i16, i32);
impl_measure!(@both u32, i32, i64);
impl_measure!(@both u32, i64, i64);
impl_measure!(@both u32, i128, i128);
impl_measure!(@both u32, isize, isize); // FIXME: only if 64-bit archi.
impl_measure!(u64, u64, u64);
impl_measure!(@both u64, u128, u128);
impl_measure!(@both u64, usize, usize);
impl_measure!(@both u64, i8, i128);
impl_measure!(@both u64, i16, i128);
impl_measure!(@both u64, i32, i128);
impl_measure!(@both u64, i64, i128);
impl_measure!(@both u64, i128, i128);
impl_measure!(u128, u128, u128);
impl_measure!(i8, i8, i8);
impl_measure!(@both i8, i16, i16);
impl_measure!(@both i8, i32, i32);
impl_measure!(@both i8, i64, i64);
impl_measure!(@both i8, i128, i128);
impl_measure!(@both i8, isize, isize);
impl_measure!(i16, i16, i16);
impl_measure!(@both i16, i32, i32);
impl_measure!(@both i16, i64, i64);
impl_measure!(@both i16, i128, i128);
impl_measure!(@both i16, isize, isize);
impl_measure!(i32, i32, i32);
impl_measure!(@both i32, i64, i64);
impl_measure!(@both i32, i128, i128);
impl_measure!(@both i32, isize, isize);
impl_measure!(i64, i64, i64);
impl_measure!(@both i64, i128, i128);
impl_measure!(@both i64, isize, isize); // FIXME: only if 64-bit archi.
impl_measure!(i128, i128, i128);
impl_measure!(usize, usize, usize);
impl_measure!(isize, isize, isize);

impl<T: PartialEq<Self>> Measure<T> for f32 {
	fn distance(&self, other: &T) -> Distance {
		if *other == *self {
			Distance::Zero
		} else {
			Distance::More
		}
	}
}

impl<T: PartialEq<Self>> Measure<T> for f64 {
	fn distance(&self, other: &T) -> Distance {
		if *other == *self {
			Distance::Zero
		} else {
			Distance::More
		}
	}
}

pub enum RangeOrdering {
	Before(bool),
	Intersecting(bool, bool),
	After(bool)
}

impl RangeOrdering {
	pub fn is_before(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::Before(c) => !c || !disconnected,
			_ => false
		}
	}

	pub fn is_after(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::After(c) => !c || !disconnected,
			_ => false
		}
	}

	pub fn matches(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::Before(c) => *c && !disconnected,
			RangeOrdering::After(c) => *c && !disconnected,
			RangeOrdering::Intersecting(_, _) => true
		}
	}
}

pub trait RangePartialOrd<T = Self> {
	fn range_partial_cmp<R: AsRange<Item=T>>(&self, other: &R) -> Option<RangeOrdering>;
}

impl<R: AsRange, U> RangePartialOrd<U> for R where R::Item: PartialOrd<U> + Measure<U> {
	fn range_partial_cmp<S: AsRange<Item=U>>(&self, other: &S) -> Option<RangeOrdering> {
		match direct_bound_partial_cmp(self.start(), other.start(), true) {
			Some(BoundOrdering::Included(limit_before)) => match inverse_bound_partial_cmp(self.start(), other.end(), false) {
				Some(BoundOrdering::Included(_)) => match direct_bound_partial_cmp(self.end(), other.end(), false) {
					Some(BoundOrdering::Included(limit_after)) => Some(RangeOrdering::Intersecting(limit_before, limit_after)),
					Some(BoundOrdering::Excluded(_)) => Some(RangeOrdering::Intersecting(limit_before, false)),
					None => None
				},
				Some(BoundOrdering::Excluded(limit_after)) => Some(RangeOrdering::After(limit_after)),
				None => None
			},
			Some(BoundOrdering::Excluded(_)) => match inverse_bound_partial_cmp(self.end(), other.start(), true) {
				Some(BoundOrdering::Included(_)) => match direct_bound_partial_cmp(self.end(), other.end(), false) {
					Some(BoundOrdering::Included(limit_after)) => Some(RangeOrdering::Intersecting(false, limit_after)),
					Some(BoundOrdering::Excluded(_)) => Some(RangeOrdering::Intersecting(false, false)),
					None => None
				},
				Some(BoundOrdering::Excluded(limit_before)) => Some(RangeOrdering::Before(limit_before)),
				None => None
			},
			None => None
		}
	}
}

pub enum BoundOrdering {
	Included(bool),
	Excluded(bool)
}

pub trait BoundPartialOrd<T = Self> {
	fn bound_partial_cmp<B: AsBound<Item = T>>(&self, other: &Directed<B>) -> Option<BoundOrdering>;
}

impl<B: AsBound, U> BoundPartialOrd<U> for Directed<B> where B::Item: PartialOrd<U> + Measure<U> {
	fn bound_partial_cmp<C: AsBound<Item = U>>(&self, other: &Directed<C>) -> Option<BoundOrdering> {
		match (self, other) {
			(Directed::Start(a), Directed::Start(b)) => direct_bound_partial_cmp(a.bound(), b.bound(), true),
			(Directed::Start(a), Directed::End(b)) => inverse_bound_partial_cmp(a.bound(), b.bound(), false),
			(Directed::End(a), Directed::Start(b)) => inverse_bound_partial_cmp(a.bound(), b.bound(), true),
			(Directed::End(a), Directed::End(b)) => direct_bound_partial_cmp(a.bound(), b.bound(), false)
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub enum Directed<T> {
	Start(T),
	End(T)
}

impl<T> Directed<Bound<T>> {
	pub fn as_ref(&self) -> Directed<Bound<&T>> {
		match self {
			Directed::Start(b) => Directed::Start(match b {
				Bound::Included(b) => Bound::Included(b),
				Bound::Excluded(b) => Bound::Excluded(b),
				Bound::Unbounded => Bound::Unbounded
			}),
			Directed::End(b) => Directed::End(match b {
				Bound::Included(b) => Bound::Included(b),
				Bound::Excluded(b) => Bound::Excluded(b),
				Bound::Unbounded => Bound::Unbounded
			})
		}
	}
}

impl<T, U> PartialEq<Directed<Bound<&U>>> for Directed<Bound<&T>> where T: Measure<U> + PartialOrd<U> {
	fn eq(&self, other: &Directed<Bound<&U>>) -> bool {
		self.partial_cmp(other) == Some(Ordering::Equal)
	}
}

impl<T, U> PartialOrd<Directed<Bound<&U>>> for Directed<Bound<&T>> where T: Measure<U> + PartialOrd<U> {
	fn partial_cmp(&self, other: &Directed<Bound<&U>>) -> Option<Ordering> {
		match self.bound_partial_cmp(other) {
			Some(BoundOrdering::Included(true)) => Some(Ordering::Equal),
			Some(BoundOrdering::Included(false)) => match other {
				Directed::Start(_) => Some(Ordering::Greater),
				Directed::End(_) => Some(Ordering::Less)
			},
			Some(BoundOrdering::Excluded(_)) => match other {
				Directed::Start(_) => Some(Ordering::Less),
				Directed::End(_) => Some(Ordering::Greater)
			},
			None => None
		}
	}
}

fn max_bound<'a, T: Measure + PartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>, start: bool) -> Bound<&'a T> {
	match direct_bound_partial_cmp(a, b, start) {
		Some(BoundOrdering::Included(_)) => b,
		_ => a
	}
}

// #[derive(Clone, Copy, PartialEq, Eq)]
// pub enum BoundOrdering {
// 	Included(bool),
// 	Excluded(bool)
// }

// pub trait BoundPartialOrd<T = Self> {
// 	fn bound_partial_cmp(&self, bound: &DirectedBound<&T>) -> Option<BoundOrdering>;
// }

// impl<T: RangeElement, U> BoundPartialOrd<U> for T where T: Measure<U> + PartialOrd<U> {
// 	fn bound_partial_cmp(&self, bound: &DirectedBound<&U>) -> Option<BoundOrdering> {
// 		match bound.direction {
// 			Direction::Start => match bound.value {
// 				Bound::Included(value) => match self.partial_cmp(value) {
// 					Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
// 					Some(Ordering::Less) => Some(BoundOrdering::Excluded(self.distance(value) == Distance::One)),
// 					Some(Ordering::Greater) => Some(BoundOrdering::Included(false)),
// 					None => None
// 				},
// 				Bound::Excluded(value) => match self.partial_cmp(value) {
// 					Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
// 					Some(Ordering::Less) => Some(BoundOrdering::Excluded(false)),
// 					Some(Ordering::Greater) => Some(BoundOrdering::Included(self.distance(value) == Distance::One)),
// 					None => None
// 				},
// 				Bound::Unbounded => Some(BoundOrdering::Included(false))
// 			},
// 			Direction::End => match bound.value {
// 				Bound::Included(value) => match self.partial_cmp(value) {
// 					Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
// 					Some(Ordering::Greater) => Some(BoundOrdering::Excluded(self.distance(value) == Distance::One)),
// 					Some(Ordering::Less) => Some(BoundOrdering::Included(false)),
// 					None => None
// 				},
// 				Bound::Excluded(value) => match self.partial_cmp(value) {
// 					Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
// 					Some(Ordering::Greater) => Some(BoundOrdering::Excluded(false)),
// 					Some(Ordering::Less) => Some(BoundOrdering::Included(self.distance(value) == Distance::One)),
// 					None => None
// 				},
// 				Bound::Unbounded => Some(BoundOrdering::Included(false))
// 			}
// 		}
// 	}
// }

fn direct_bound_partial_cmp<T, U>(b1: Bound<&T>, b2: Bound<&U>, start: bool) -> Option<BoundOrdering> where T: Measure<U> + PartialOrd<U> {
	let included_ord = if start {
		Ordering::Greater
	} else {
		Ordering::Less
	};

	match (b1, b2) {
		(Bound::Included(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => Some(BoundOrdering::Excluded(v1.distance(v2) == Distance::One)),
			None => None
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(v1.distance(v2) == Distance::One)),
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None
		},
		(Bound::Included(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(false)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => match v1.distance(v2) {
				Distance::One => Some(BoundOrdering::Included(true)),
				Distance::Two => Some(BoundOrdering::Excluded(true)),
				_ => Some(BoundOrdering::Excluded(false))
			},
			None => None
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => Some(BoundOrdering::Excluded(v1.distance(v2) == Distance::One)),
			None => None
		},
		(Bound::Excluded(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Unbounded, Bound::Included(_)) => Some(BoundOrdering::Excluded(false)),
		(Bound::Unbounded, Bound::Excluded(_)) => Some(BoundOrdering::Excluded(false)),
		(Bound::Unbounded, Bound::Unbounded) => Some(BoundOrdering::Included(true))
	}
}

fn inverse_bound_partial_cmp<T, U>(b1: Bound<&T>, b2: Bound<&U>, start: bool) -> Option<BoundOrdering> where T: Measure<U> + PartialOrd<U> {
	let included_ord = if start {
		Ordering::Greater
	} else {
		Ordering::Less
	};
	
	match (b1, b2) {
		(Bound::Included(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => Some(BoundOrdering::Excluded(v1.distance(v2) == Distance::One)),
			None => None
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(v1.distance(v2) == Distance::One)),
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None
		},
		(Bound::Included(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)), // []v2=v1
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(v1.distance(v2) == Distance::One)),
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(false)),
			Some(ord) if ord == included_ord => match v1.distance(v2) {
				Distance::One => Some(BoundOrdering::Excluded(true)), // v2 [] v1
				Distance::Two => Some(BoundOrdering::Included(true)), // v2 [ x ] v1
				_ => Some(BoundOrdering::Included(false)) // v2 [ x .. y ] v1
			},
			Some(_) => Some(BoundOrdering::Excluded(false)), // ] v1 v2 [ 
			None => None
		},
		(Bound::Excluded(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Unbounded, Bound::Included(_)) => Some(BoundOrdering::Included(false)),
		(Bound::Unbounded, Bound::Excluded(_)) => Some(BoundOrdering::Included(false)),
		(Bound::Unbounded, Bound::Unbounded) => Some(BoundOrdering::Included(false))
	}
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
fn is_range_empty<T, U>(start: Bound<&T>, end: Bound<&U>) -> bool where T: PartialOrd<U> + Measure<U> {
	Directed::Start(start) > Directed::End(end)
}

#[cfg(test)]
mod tests {
	use super::*;

	macro_rules! make_bound {
		([= $v:literal ..]) => { Directed::Start(Bound::Included(&$v)) };
		([$v:literal ..]) => { Directed::Start(Bound::Excluded(&$v)) };
		([~ ..]) => { Directed::Start(Bound::Unbounded) };
		([..= $v:literal]) => { Directed::End(Bound::Included(&$v)) };
		([.. $v:literal]) => { Directed::End(Bound::Excluded(&$v)) };
		([.. ~]) => { Directed::End(Bound::Unbounded) }
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