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
pub struct DirectedBound<T> {
	value: Bound<T>,
	direction: BoundDirection
}

impl<T> DirectedBound<T> {
	pub fn new(bound: Bound<T>, direction: BoundDirection) -> DirectedBound<T> {
		DirectedBound {
			value: bound,
			direction
		}
	}

	pub fn start(bound: Bound<T>) -> DirectedBound<T> {
		DirectedBound {
			value: bound,
			direction: BoundDirection::Start
		}
	}

	pub fn end(bound: Bound<T>) -> DirectedBound<T> {
		DirectedBound {
			value: bound,
			direction: BoundDirection::End
		}
	}

	pub fn as_ref(&self) -> DirectedBound<&T> {
		DirectedBound {
			value: match &self.value {
				Bound::Included(b) => Bound::Included(b),
				Bound::Excluded(b) => Bound::Excluded(b),
				Bound::Unbounded => Bound::Unbounded
			},
			direction: self.direction
		}
	}
}

macro_rules! impl_int_bound_partial_order {
	($ty:ident) => {
		impl<T: PartialOrd<$ty>> BoundPartialOrd<T> for $ty {
			fn bound_partial_cmp(&self, bound: DirectedBound<&T>) -> Option<BoundOrdering> {
				match bound.value {
					Bound::Included(b) => match b.partial_cmp(self) {
						Some(Ordering::Less) => if bound.direction == BoundDirection::Start {
							Some(BoundOrdering::Included(false))
						} else {
							let limit = *self > std::$ty::MIN && *b == *self - 1;
							Some(BoundOrdering::Excluded(limit))
						},
						Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
						Some(Ordering::Greater) => if bound.direction == BoundDirection::Start {
							let limit = *self < std::$ty::MAX && *b == *self + 1;
							Some(BoundOrdering::Excluded(limit))
						} else {
							Some(BoundOrdering::Included(false))
						},
						None => None
					},
					Bound::Excluded(b) => match b.partial_cmp(self) {
						Some(Ordering::Less) => if bound.direction == BoundDirection::Start {
							let limit = *self > std::$ty::MIN && *b == *self - 1;
							Some(BoundOrdering::Included(limit))
						} else {
							Some(BoundOrdering::Excluded(false))
						},
						Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
						Some(Ordering::Greater) => if bound.direction == BoundDirection::Start {
							Some(BoundOrdering::Excluded(false))
						} else {
							let limit = *self < std::$ty::MAX && *b == *self + 1;
							Some(BoundOrdering::Included(limit))
						},
						None => None
					},
					Bound::Unbounded => {
						let limit = (bound.direction == BoundDirection::Start && *self == std::$ty::MIN) || (bound.direction == BoundDirection::End && *self == std::$ty::MAX);
						Some(BoundOrdering::Included(limit))
					}
				}
			}
		}
	}
}

impl_int_bound_partial_order!(u8);
impl_int_bound_partial_order!(i8);
impl_int_bound_partial_order!(u16);
impl_int_bound_partial_order!(i16);
impl_int_bound_partial_order!(u32);
impl_int_bound_partial_order!(i32);
impl_int_bound_partial_order!(u64);
impl_int_bound_partial_order!(i64);
impl_int_bound_partial_order!(u128);
impl_int_bound_partial_order!(i128);
impl_int_bound_partial_order!(usize);
impl_int_bound_partial_order!(isize);

macro_rules! impl_float_bound_partial_order {
	($ty:ident) => {
		impl<T: PartialOrd<$ty>> BoundPartialOrd<T> for $ty {
			fn bound_partial_cmp(&self, bound: DirectedBound<&T>) -> Option<BoundOrdering> {
				match bound.value {
					Bound::Included(b) => match b.partial_cmp(self) {
						Some(Ordering::Less) => if bound.direction == BoundDirection::Start {
							Some(BoundOrdering::Included(false))
						} else {
							Some(BoundOrdering::Excluded(false))
						},
						Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
						Some(Ordering::Greater) => if bound.direction == BoundDirection::Start {
							Some(BoundOrdering::Excluded(false))
						} else {
							Some(BoundOrdering::Included(false))
						},
						None => None
					},
					Bound::Excluded(b) => match b.partial_cmp(self) {
						Some(Ordering::Less) => if bound.direction == BoundDirection::Start {
							Some(BoundOrdering::Included(false))
						} else {
							Some(BoundOrdering::Excluded(false))
						},
						Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
						Some(Ordering::Greater) => if bound.direction == BoundDirection::Start {
							Some(BoundOrdering::Excluded(false))
						} else {
							Some(BoundOrdering::Included(false))
						},
						None => None
					},
					Bound::Unbounded => {
						let limit = (bound.direction == BoundDirection::Start && *self == std::$ty::MIN) || (bound.direction == BoundDirection::End && *self == std::$ty::MAX);
						Some(BoundOrdering::Included(limit))
					}
				}
			}
		}
	}
}

impl_float_bound_partial_order!(f32);
impl_float_bound_partial_order!(f64);

impl<T, U> PartialEq<DirectedBound<U>> for DirectedBound<T> where T: BoundPartialOrd<U> {
	fn eq(&self, other: &DirectedBound<U>) -> bool {
		self.partial_cmp(other) == Some(Ordering::Equal)
	}
}

impl<T, U> PartialOrd<DirectedBound<U>> for DirectedBound<T> where T: BoundPartialOrd<U> {
	fn partial_cmp(&self, other: &DirectedBound<U>) -> Option<Ordering> {
		match &self.value {
			Bound::Included(a) => match a.bound_partial_cmp(other.as_ref()) {
				Some(BoundOrdering::Included(true)) => Some(Ordering::Equal),
				Some(BoundOrdering::Included(false)) => if other.direction == BoundDirection::Start {
					Some(Ordering::Greater)
				} else {
					Some(Ordering::Less)
				},
				Some(BoundOrdering::Excluded(_)) => if other.direction == BoundDirection::Start {
					Some(Ordering::Less)
				} else {
					Some(Ordering::Greater)
				},
				None => None
			},
			Bound::Excluded(a) => match a.bound_partial_cmp(other.as_ref()) {
				Some(BoundOrdering::Included(_)) => if self.direction == BoundDirection::Start {
					Some(Ordering::Greater)
				} else {
					Some(Ordering::Less)
				},
				Some(BoundOrdering::Excluded(true)) => match (self.direction, other.direction) {
					(BoundDirection::Start, BoundDirection::End) => Some(Ordering::Greater),
					(BoundDirection::End, BoundDirection::Start) => Some(Ordering::Less),
					_ => Some(Ordering::Equal)
				},
				Some(BoundOrdering::Excluded(false)) => if other.direction == BoundDirection::Start {
					Some(Ordering::Less)
				} else {
					Some(Ordering::Greater)
				},
				None => None
			},
			Bound::Unbounded => match &other.value {
				Bound::Unbounded => self.direction.partial_cmp(&other.direction),
				_ => if self.direction == BoundDirection::Start {
					Some(Ordering::Less)
				} else {
					Some(Ordering::Greater)
				}
			}
		}
	}
}

// pub fn bounds_patial_cmp<T, U>(a: DirectedBound<T>, b: DirectedBound<U>) -> Option<Ordering> where T: BoundPartialOrd<U> {
// 	match a.value {
// 		Bound::Included(a) => match a.bound_partial_cmp(b) {
// 			Some(BoundOrdering::Included) => if b.direction == BoundDirection::Start {
// 				Some(Ordering::Greater)
// 			} else {
// 				Some(Ordering::Less)
// 			},
// 			// ...
// 		}
// 	}
// }

fn max_bound<'a, T: BoundPartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>, direction: BoundDirection) -> Bound<&'a T> {
	match &a {
		Bound::Included(value) => match value.bound_partial_cmp(DirectedBound::new(b, direction)) {
			Some(BoundOrdering::Included(_)) => b,
			_ => a
		},
		Bound::Excluded(value) => match value.bound_partial_cmp(DirectedBound::new(b, direction)) {
			Some(BoundOrdering::Included(_)) => b,
			_ => a
		},
		Bound::Unbounded => a
	}
	
}

#[derive(Debug, Clone)]
pub struct Range<T> {
	start: Bound<T>,
	end: Bound<T>
}

impl<T> Range<T> {
	pub fn add<S>(&mut self, other: &S) where T: Clone + BoundPartialOrd, S: RangeBounds<T> {
		self.start = max_bound(self.start_bound(), other.start_bound(), BoundDirection::Start).cloned();
		self.end = max_bound(self.end_bound(), other.end_bound(), BoundDirection::End).cloned();
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
pub enum BoundOrdering {
	Included(bool),
	Excluded(bool)
}

pub trait BoundPartialOrd<T = Self> {
	fn bound_partial_cmp(&self, bound: DirectedBound<&T>) -> Option<BoundOrdering>;
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RangeOrdering {
	Before(bool),
	Intersecting(bool, bool),
	After(bool)
}

impl RangeOrdering {
	pub fn is_before(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::Before(limit) => !(disconnected && *limit),
			_ => false
		}
	}

	pub fn is_after(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::After(limit) => !(disconnected && *limit),
			_ => false
		}
	}

	pub fn matches(&self, disconnected: bool) -> bool {
		match self {
			RangeOrdering::Intersecting(limit_before, limit_after) => !(disconnected && *limit_before) && !(disconnected && *limit_after),
			_ => false
		}
	}
}

pub trait RangePartialOrd<T = Self>: Sized {
	fn range_partial_cmp<R: RangeBounds<T>>(&self, range: &R) -> Option<RangeOrdering>;
}

impl<T: Copy, U> RangePartialOrd<U> for T where T: BoundPartialOrd<U> {
	fn range_partial_cmp<R: RangeBounds<U>>(&self, range: &R) -> Option<RangeOrdering> {
		match self.bound_partial_cmp(DirectedBound::start(range.start_bound())) {
			Some(BoundOrdering::Excluded(limit_before)) => Some(RangeOrdering::Before(limit_before)),
			Some(BoundOrdering::Included(limit_before)) => match self.bound_partial_cmp(DirectedBound::end(range.end_bound())) {
				Some(BoundOrdering::Excluded(limit_after)) => Some(RangeOrdering::After(limit_after)),
				Some(BoundOrdering::Included(limit_after)) => Some(RangeOrdering::Intersecting(limit_before, limit_after)),
				None => None
			},
			None => None
		}
	}
}

impl<T, U> RangePartialOrd<U> for Range<T> where T: BoundPartialOrd<U> {
	fn range_partial_cmp<R: RangeBounds<U>>(&self, _range: &R) -> Option<RangeOrdering> {
		panic!("TODO")
	}
}

// impl<U, T: BoundPartialOrd<U>> PartialEq<std::ops::Range<U>> for Range<T> {
// 	fn eq(&self, other: &std::ops::Range<U>) -> bool {
// 		T::bound_partial_cmp((self.start_bound(), BoundDirection::Start), (other.start_bound(), BoundDirection::Start)) == Some(Ordering::Equal) &&
// 		T::bound_partial_cmp((self.end_bound(), BoundDirection::End), (other.end_bound(), BoundDirection::End)) == Some(Ordering::Equal)
// 	}
// }

// impl<U, T: BoundPartialOrd<U>> PartialOrd<std::ops::Range<U>> for Range<T> {
// 	fn partial_cmp(&self, other: &std::ops::Range<U>) -> Option<Ordering> {
// 		//T::bound_partial_cmp((self.start_bound(), BoundDirection::Start), (other.start_bound(), BoundDirection::Start))
// 		None
// 	}
// }

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
	
	fn intersects<U: Clone, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U>;

	fn connected_to<U: Clone, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U>;

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
fn is_range_empty<T: Clone, U: Clone>(start: Bound<&T>, end: Bound<&U>) -> bool where T: BoundPartialOrd<U> {
	DirectedBound::start(start.cloned()) > DirectedBound::end(end.cloned())
}

#[inline(always)]
fn are_conneted_bounds<T: Clone, U: Clone>(end: Bound<&T>, start: Bound<&U>) -> bool where T: BoundPartialOrd<U> {
	match (invert_bound(end), invert_bound(start)) {
		(Some(start), Some(end)) => is_range_empty(start, end),
		_ => true
	}
}

impl<T: Clone, R: RangeBounds<T>> RangeExt<T> for R {
	fn is_empty(&self) -> bool where T: BoundPartialOrd {
		is_range_empty(self.start_bound(), self.end_bound())
	}

	fn intersects<U: Clone, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U> {
		!is_range_empty(self.start_bound(), other.end_bound()) || !is_range_empty(other.start_bound(), self.end_bound())
	}

	fn connected_to<U: Clone, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U> {
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
		([= $v:literal ..]) => { DirectedBound::start(Bound::Included($v)) };
		([$v:literal ..]) => { DirectedBound::start(Bound::Excluded($v)) };
		([~ ..]) => { DirectedBound::start(Bound::Unbounded) };
		([..= $v:literal]) => { DirectedBound::end(Bound::Included($v)) };
		([.. $v:literal]) => { DirectedBound::end(Bound::Excluded($v)) };
		([.. ~]) => { DirectedBound::end(Bound::Unbounded) }
	}

	macro_rules! test_bound_cmp {
		(@assert $ty:ty, $a:tt, $b:tt, $expected:ident) => {
			assert_eq!(<DirectedBound<$ty> as PartialOrd>::partial_cmp(&make_bound!($a), &make_bound!($b)), Some(Ordering::$expected));
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
