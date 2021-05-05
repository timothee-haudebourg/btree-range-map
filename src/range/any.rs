use std::{
	ops::RangeBounds,
	hash::{
		Hash,
		Hasher
	}
};
use crate::util::{
	Saturating,
	PartialEnum
};
use super::{
	Bound,
	AsRange,
	Directed,
	Measure,
	is_range_empty,
	max_bound,
	direct_bound_partial_eq
};

#[derive(Debug, Clone, Copy)]
pub struct AnyRange<T> {
	pub start: Bound<T>,
	pub end: Bound<T>
}

impl<T> AnyRange<T> {
	pub fn from<R: AsRange<Item=T>>(range: R) -> AnyRange<T> where T: Clone {
		AnyRange {
			start: range.start().cloned(),
			end: range.end().cloned()
		}
	}

	pub fn is_empty(&self) -> bool where T: PartialOrd + Measure {
		is_range_empty(self.start_bound(), self.end_bound())
	}

	pub fn len(&self) -> Saturating<T::Len> where T: Measure {
		match (self.start_bound(), self.end_bound()) {
			(Bound::Included(a), Bound::Included(b)) => a.distance(Saturating::Sub(b)) + b.len(),
			(Bound::Included(a), Bound::Excluded(b)) => a.distance(Saturating::Sub(b)),
			(Bound::Included(a), Bound::Unbounded) => a.distance(Saturating::Saturated),
			(Bound::Excluded(a), Bound::Included(b)) => a.distance(Saturating::Sub(b)) - a.len() + b.len(),
			(Bound::Excluded(a), Bound::Excluded(b)) => a.distance(Saturating::Sub(b)) - a.len(),
			(Bound::Excluded(a), Bound::Unbounded) => a.distance(Saturating::Saturated) - a.len(),
			(Bound::Unbounded, Bound::Included(b)) => T::MIN.distance(Saturating::Sub(b)) + b.len(),
			(Bound::Unbounded, Bound::Excluded(b)) => T::MIN.distance(Saturating::Sub(b)),
			(Bound::Unbounded, Bound::Unbounded) => T::MIN.distance(Saturating::Saturated)
		}
	}

	/// Get the first element of the range if there is one.
	pub fn first(&self) -> Option<T> where T: PartialOrd + Clone + Measure {
		if self.is_empty() {
			None
		} else {
			match self.start_bound() {
				Bound::Included(a) => Some(a.clone()),
				Bound::Excluded(a) => a.succ(),
				Bound::Unbounded => Some(T::MIN)
			}
		}
	}

	/// Get the last element of the range if there is one.
	pub fn last(&self) -> Option<T> where T: PartialOrd + Clone + Measure {
		if self.is_empty() {
			None
		} else {
			match self.end_bound() {
				Bound::Included(a) => Some(a.clone()),
				Bound::Excluded(a) => a.pred(),
				Bound::Unbounded => Some(T::MAX)
			}
		}
	}

	pub fn add<S>(&mut self, other: &S) where T: Clone + Measure + PartialOrd, S: RangeBounds<T> {
		self.start = max_bound(self.start_bound(), other.start_bound(), true).cloned();
		self.end = max_bound(self.end_bound(), other.end_bound(), false).cloned();
	}

	pub fn intersects<S>(&self, other: &S) -> bool where T: Measure + PartialOrd, S: RangeBounds<T> {
		Directed::End(self.end_bound()) > Directed::Start(other.start_bound()) && Directed::End(other.end_bound()) > Directed::Start(self.start_bound())
	}

	pub fn pick_in_intersection<S>(&self, other: &S) -> Option<T> where T: Measure + Clone + PartialOrd, S: AsRange + RangeBounds<T> {
		if self.intersects(other) {
			if Directed::End(self.end_bound()) <= Directed::End(other.end_bound()) {
				if other.is_empty() {
					None
				} else {
					// pick between other.start and self.end
					Some(match other.start_bound() {
						Bound::Included(a) => a.clone(),
						Bound::Excluded(a) => a.succ().unwrap(),
						Bound::Unbounded => T::MIN
					})
				}
			} else {
				if self.is_empty() {
					None
				} else {
					// pick between self.start and other.end
					Some(match self.start_bound() {
						Bound::Included(a) => a.clone(),
						Bound::Excluded(a) => a.succ().unwrap(),
						Bound::Unbounded => T::MIN
					})
				}
			}
		} else {
			None
		}
	}
}

impl<T, U> PartialEq<AnyRange<U>> for AnyRange<T> where T: Measure<U> + PartialOrd<U> {
	fn eq(&self, other: &AnyRange<U>) -> bool {
		direct_bound_partial_eq(self.start_bound(), other.start_bound(), true) &&
		direct_bound_partial_eq(self.end_bound(), other.end_bound(), false)
	}
}

impl<T> Eq for AnyRange<T> where T: Measure + Ord {}

impl<T> Hash for AnyRange<T> where T: Hash + PartialEnum {
	fn hash<H: Hasher>(&self, h: &mut H) {
		Directed::Start(self.start_bound()).hash(h);
		Directed::End(self.end_bound()).hash(h);
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
		match &self.end {
			Bound::Included(v) => Bound::Included(v),
			Bound::Excluded(v) => Bound::Excluded(v),
			Bound::Unbounded => Bound::Unbounded
		}
	}
}