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
					(Bound::Excluded(a), Bound::Excluded(b)) => a.partial_cmp(b),
					(Bound::Excluded(_), Bound::Unbounded) => if b_direction == BoundDirection::Start {
						Some(Ordering::Greater)
					} else {
						Some(Ordering::Less)
					},
					(Bound::Unbounded, Bound::Unbounded) => Some(Ordering::Equal),
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

#[derive(Debug, Clone, Copy)]
pub struct Range<T> {
	start: Bound<T>,
	end: Bound<T>
}

pub trait RangeExt<T> {
	fn is_empty(&self) -> bool where T: BoundPartialOrd;
	
	fn intersects<U, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U>;

	fn connected_to<U, S>(&self, other: &S) -> bool where T: BoundPartialOrd<U>, U: BoundPartialOrd<T>, S: RangeBounds<U>;

	// Output ranges are never empty.
	fn without<'a, S>(&'a self, other: &'a S) -> (Option<Range<&'a T>>, Option<Range<&'a T>>) where T: BoundPartialOrd, S: RangeBounds<T>;

	// fn add<S>(&mut self, other: &S) where T: PartialOrd + Clone, S: RangeBounds<T>;
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

impl<T: Clone, R: RangeBounds<T>> RangeExt<T> for R {
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

	#[test]
	fn integer_bound_partial_less() {
		assert_eq!( // =0.. < =1..
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Included(&1), BoundDirection::Start)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // =0.. < 0..
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Excluded(&0), BoundDirection::Start)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // =0.. < ..=1
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Included(&1), BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // =0.. < ..2
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Excluded(&2), BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // =0.. < ..-
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Unbounded, BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // 0.. < =2..
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Excluded(&0), BoundDirection::Start),
				(Bound::Included(&2), BoundDirection::Start)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // 0.. < 1..
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Excluded(&0), BoundDirection::Start),
				(Bound::Excluded(&1), BoundDirection::Start)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // 0.. < ..=2
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Excluded(&0), BoundDirection::Start),
				(Bound::Included(&2), BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // 0.. < ..3
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Excluded(&0), BoundDirection::Start),
				(Bound::Excluded(&3), BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // 0.. < ..-
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Excluded(&0), BoundDirection::Start),
				(Bound::Unbounded, BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // -.. < =0..
			<i32 as BoundPartialOrd>::bound_partial_cmp(
				(Bound::Unbounded, BoundDirection::Start),
				(Bound::Included(&0), BoundDirection::Start)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // -.. < 0..
			<i32 as BoundPartialOrd>::bound_partial_cmp(
				(Bound::Unbounded, BoundDirection::Start),
				(Bound::Excluded(&0), BoundDirection::Start)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // -.. < ..=0
			<i32 as BoundPartialOrd>::bound_partial_cmp(
				(Bound::Unbounded, BoundDirection::Start),
				(Bound::Included(&0), BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // -.. < ..0
			<i32 as BoundPartialOrd>::bound_partial_cmp(
				(Bound::Unbounded, BoundDirection::Start),
				(Bound::Excluded(&0), BoundDirection::End)
			),
			Some(Ordering::Less)
		);
		assert_eq!( // -.. < ..-
			<i32 as BoundPartialOrd>::bound_partial_cmp(
				(Bound::Unbounded, BoundDirection::Start),
				(Bound::Unbounded, BoundDirection::End)
			),
			Some(Ordering::Less)
		);
	}

	#[test]
	fn integer_bound_partial_eq() {
		assert_eq!( // =0.. == =0..
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Included(&0), BoundDirection::Start)
			),
			Some(Ordering::Equal)
		);
		assert_eq!( // =1.. == 0..
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&1), BoundDirection::Start),
				(Bound::Excluded(&0), BoundDirection::Start)
			),
			Some(Ordering::Equal)
		);
		assert_eq!( // =0.. == ..=0
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Included(&0), BoundDirection::End)
			),
			Some(Ordering::Equal)
		);
		assert_eq!( // =0.. == ..1
			BoundPartialOrd::bound_partial_cmp(
				(Bound::Included(&0), BoundDirection::Start),
				(Bound::Excluded(&1), BoundDirection::End)
			),
			Some(Ordering::Equal)
		);
	}

	#[test]
	fn intersection() {
		assert!((0..10).intersects(&(5..100)));
	}

	// Intersecting ranges are connected.
	#[test]
	fn connected_intersection() {
		assert!((0..10).connected_to(&(5..100)));
	}

	#[test]
	fn connected() {
		assert!((0..10).connected_to(&(10..20)));
		assert!((10..20).connected_to(&(0..10)));
	}

	#[test]
	fn disconnected() {
		assert!(!(0..10).connected_to(&(11..20)));
		assert!(!(11..20).connected_to(&(0..10)));
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

// pub trait RangeOrd<T: Clone + Ord, U> {
// 	fn cmp(a: &ExclusiveRange<T>, b: U) -> Ordering;

// 	fn matches(a: &ExclusiveRange<T>, b: U) -> bool;
// }