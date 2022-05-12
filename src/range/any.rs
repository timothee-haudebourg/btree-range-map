use super::{
	direct_bound_cmp, direct_bound_partial_cmp, direct_bound_partial_eq, is_range_empty, max_bound,
	min_bound, AsRange, Bound, BoundOrdering, Directed, Measure,
};
use range_traits::{Bounded, PartialEnum};
use std::{
	cmp::{Ord, Ordering, PartialOrd},
	fmt,
	hash::{Hash, Hasher},
	ops::RangeBounds,
};

#[derive(Clone, Copy)]
pub struct AnyRange<T> {
	pub start: Bound<T>,
	pub end: Bound<T>,
}

impl<T> AnyRange<T> {
	pub fn from<R: AsRange<Item = T>>(range: R) -> AnyRange<T>
	where
		T: Clone,
	{
		AnyRange {
			start: range.start().cloned(),
			end: range.end().cloned(),
		}
	}

	pub fn into_bounds(self) -> (Bound<T>, Bound<T>) {
		(self.start, self.end)
	}

	pub fn is_empty(&self) -> bool
	where
		T: Measure + PartialEnum,
	{
		is_range_empty(self.start_bound(), self.end_bound())
	}

	pub fn len(&self) -> T::Len
	where
		T: Measure + Bounded,
	{
		match (self.start_bound(), self.end_bound()) {
			(Bound::Included(a), Bound::Included(b)) => a.distance(b) + b.len(),
			(Bound::Included(a), Bound::Excluded(b)) => a.distance(b),
			(Bound::Included(a), Bound::Unbounded) => a.distance(&T::MAX),
			(Bound::Excluded(a), Bound::Included(b)) => a.distance(b) - a.len() + b.len(),
			(Bound::Excluded(a), Bound::Excluded(b)) => a.distance(b) - a.len(),
			(Bound::Excluded(a), Bound::Unbounded) => a.distance(&T::MAX) - a.len(),
			(Bound::Unbounded, Bound::Included(b)) => T::MIN.distance(b) + b.len(),
			(Bound::Unbounded, Bound::Excluded(b)) => T::MIN.distance(b),
			(Bound::Unbounded, Bound::Unbounded) => T::MIN.distance(&T::MAX),
		}
	}

	pub fn pick(&self) -> Option<T>
	where
		T: Clone + Measure + PartialEnum + Bounded,
	{
		self.first().or_else(|| self.last())
	}

	/// Get the first element of the range if there is one.
	pub fn first(&self) -> Option<T>
	where
		T: Clone + Measure + PartialEnum + Bounded,
	{
		if self.is_empty() {
			None
		} else {
			match self.start_bound() {
				Bound::Included(a) => Some(a.clone()),
				Bound::Excluded(a) => a.succ(),
				Bound::Unbounded => Some(T::MIN),
			}
		}
	}

	/// Get the last element of the range if there is one.
	pub fn last(&self) -> Option<T>
	where
		T: Clone + Measure + PartialEnum + Bounded,
	{
		if self.is_empty() {
			None
		} else {
			match self.end_bound() {
				Bound::Included(a) => Some(a.clone()),
				Bound::Excluded(a) => a.pred(),
				Bound::Unbounded => Some(T::MAX),
			}
		}
	}

	pub fn add<S>(&mut self, other: &S)
	where
		T: Clone + Measure + PartialEnum,
		S: RangeBounds<T>,
	{
		self.start = min_bound(self.start_bound(), other.start_bound(), true).cloned();
		self.end = max_bound(self.end_bound(), other.end_bound(), false).cloned();
	}

	pub fn intersects<S>(&self, other: &S) -> bool
	where
		T: Measure + PartialEnum,
		S: RangeBounds<T>,
	{
		Directed::End(self.end_bound()) > Directed::Start(other.start_bound())
			&& Directed::End(other.end_bound()) > Directed::Start(self.start_bound())
	}

	pub fn pick_in_intersection<S>(&self, other: &S) -> Option<T>
	where
		T: Clone + Measure + PartialEnum,
		S: AsRange + RangeBounds<T>,
	{
		if self.intersects(other) {
			if Directed::End(self.end_bound()) <= Directed::End(other.end_bound()) {
				if other.is_empty() {
					None
				} else {
					// pick between other.start and self.end
					Some(match other.start_bound() {
						Bound::Included(a) => a.clone(),
						Bound::Excluded(a) => a.succ().unwrap(),
						Bound::Unbounded => T::MIN,
					})
				}
			} else if self.is_empty() {
				None
			} else {
				// pick between self.start and other.end
				Some(match self.start_bound() {
					Bound::Included(a) => a.clone(),
					Bound::Excluded(a) => a.succ().unwrap(),
					Bound::Unbounded => T::MIN,
				})
			}
		} else {
			None
		}
	}
}

impl<T: fmt::Debug> fmt::Debug for AnyRange<T> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match &self.start {
			Bound::Included(v) => v.fmt(f)?,
			Bound::Excluded(v) => write!(f, "{:?}.", v)?,
			Bound::Unbounded => write!(f, ".")?,
		}

		match &self.end {
			Bound::Included(v) => write!(f, "..={:?}", v),
			Bound::Excluded(v) => write!(f, "..{:?}", v),
			Bound::Unbounded => write!(f, ".."),
		}
	}
}

impl<'a, T> AnyRange<&'a T> {
	pub fn ref_is_empty(&self) -> bool
	where
		T: PartialEnum + Measure,
	{
		is_range_empty(self.start_bound().cloned(), self.end_bound().cloned())
	}
}

impl<T, U> PartialEq<AnyRange<U>> for AnyRange<T>
where
	T: Measure<U> + PartialOrd<U> + PartialEnum,
	U: PartialEnum,
{
	fn eq(&self, other: &AnyRange<U>) -> bool {
		direct_bound_partial_eq(self.start_bound(), other.start_bound(), true)
			&& direct_bound_partial_eq(self.end_bound(), other.end_bound(), false)
	}
}

impl<T> Eq for AnyRange<T> where T: Measure + PartialEnum + Ord {}

impl<T, U> PartialOrd<AnyRange<U>> for AnyRange<T>
where
	T: Measure<U> + PartialOrd<U> + PartialEnum,
	U: PartialEnum,
{
	fn partial_cmp(&self, other: &AnyRange<U>) -> Option<Ordering> {
		// Directed::Start(self.start_bound()).partial_cmp(Directed::Start(other.start_bound()))
		match direct_bound_partial_cmp(self.start_bound(), other.start_bound(), true) {
			Some(BoundOrdering::Excluded(_)) => Some(Ordering::Less),
			Some(BoundOrdering::Included(false)) => Some(Ordering::Greater),
			Some(BoundOrdering::Included(true)) => {
				match direct_bound_partial_cmp(self.end_bound(), other.end_bound(), false) {
					Some(BoundOrdering::Excluded(_)) => Some(Ordering::Greater),
					Some(BoundOrdering::Included(false)) => Some(Ordering::Less),
					Some(BoundOrdering::Included(true)) => Some(Ordering::Equal),
					None => None,
				}
			}
			None => None,
		}
	}
}

impl<T> Ord for AnyRange<T>
where
	T: Measure + PartialEnum + Ord,
{
	fn cmp(&self, other: &Self) -> Ordering {
		// Directed::Start(self.start_bound()).partial_cmp(Directed::Start(other.start_bound()))
		match direct_bound_cmp(self.start_bound(), other.start_bound(), true) {
			BoundOrdering::Excluded(_) => Ordering::Less,
			BoundOrdering::Included(false) => Ordering::Greater,
			BoundOrdering::Included(true) => {
				match direct_bound_cmp(self.end_bound(), other.end_bound(), false) {
					BoundOrdering::Excluded(_) => Ordering::Greater,
					BoundOrdering::Included(false) => Ordering::Less,
					BoundOrdering::Included(true) => Ordering::Equal,
				}
			}
		}
	}
}

impl<T> Hash for AnyRange<T>
where
	T: Hash + PartialEnum,
{
	fn hash<H: Hasher>(&self, h: &mut H) {
		Directed::Start(self.start_bound()).hash(h);
		Directed::End(self.end_bound()).hash(h);
	}
}

impl<T: Clone> AnyRange<&T> {
	pub fn cloned(self) -> AnyRange<T> {
		AnyRange {
			start: self.start.cloned(),
			end: self.end.cloned(),
		}
	}
}

impl<T> RangeBounds<T> for AnyRange<T> {
	fn start_bound(&self) -> Bound<&T> {
		match &self.start {
			Bound::Included(v) => Bound::Included(v),
			Bound::Excluded(v) => Bound::Excluded(v),
			Bound::Unbounded => Bound::Unbounded,
		}
	}

	fn end_bound(&self) -> Bound<&T> {
		match &self.end {
			Bound::Included(v) => Bound::Included(v),
			Bound::Excluded(v) => Bound::Excluded(v),
			Bound::Unbounded => Bound::Unbounded,
		}
	}
}
