use super::{
	direct_bound_cmp, direct_bound_partial_cmp, direct_bound_partial_eq, is_range_empty, max_bound,
	min_bound, AsRange, Bound, BoundOrdering, Directed, Measure,
};
use range_traits::{Bounded, MaybeBounded, PartialEnum};
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
	pub fn new(start: Bound<T>, end: Bound<T>) -> Self {
		Self { start, end }
	}

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
			(Bound::Included(a), Bound::Unbounded) => a.distance(&T::max()),
			(Bound::Excluded(a), Bound::Included(b)) => a.distance(b) - a.len() + b.len(),
			(Bound::Excluded(a), Bound::Excluded(b)) => a.distance(b) - a.len(),
			(Bound::Excluded(a), Bound::Unbounded) => a.distance(&T::max()) - a.len(),
			(Bound::Unbounded, Bound::Included(b)) => T::min().distance(b) + b.len(),
			(Bound::Unbounded, Bound::Excluded(b)) => T::min().distance(b),
			(Bound::Unbounded, Bound::Unbounded) => T::min().distance(&T::max()),
		}
	}

	pub fn bounded_len(&self) -> Option<T::Len>
	where
		T: Measure + MaybeBounded,
	{
		match (self.start_bound(), self.end_bound()) {
			(Bound::Included(a), Bound::Included(b)) => Some(a.distance(b) + b.len()),
			(Bound::Included(a), Bound::Excluded(b)) => Some(a.distance(b)),
			(Bound::Included(a), Bound::Unbounded) => T::max().map(|m| a.distance(&m)),
			(Bound::Excluded(a), Bound::Included(b)) => Some(a.distance(b) - a.len() + b.len()),
			(Bound::Excluded(a), Bound::Excluded(b)) => Some(a.distance(b) - a.len()),
			(Bound::Excluded(a), Bound::Unbounded) => T::max().map(|m| a.distance(&m) - a.len()),
			(Bound::Unbounded, Bound::Included(b)) => T::min().map(|m| m.distance(b) + b.len()),
			(Bound::Unbounded, Bound::Excluded(b)) => T::min().map(|m| m.distance(b)),
			(Bound::Unbounded, Bound::Unbounded) => {
				T::min().and_then(|min| T::max().map(|max| min.distance(&max)))
			}
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
				Bound::Unbounded => Some(<T as Bounded>::min()),
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
				Bound::Unbounded => Some(<T as Bounded>::max()),
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
		Directed::End(self.end_bound()) >= Directed::Start(other.start_bound())
			&& Directed::End(other.end_bound()) >= Directed::Start(self.start_bound())
	}

	pub fn intersection(&self, other: &Self) -> Self
	where
		T: Clone + Measure + PartialEnum,
	{
		Self {
			start: max_bound(self.start_bound(), other.start_bound(), true).cloned(),
			end: min_bound(self.end_bound(), other.end_bound(), false).cloned(),
		}
	}

	// pub fn pick_in_intersection<S>(&self, other: &S) -> Option<T>
	// where
	// 	T: Clone + Measure + PartialEnum,
	// 	S: AsRange + RangeBounds<T>,
	// {
	// 	if self.intersects(other) {
	// 		if Directed::End(self.end_bound()) <= Directed::End(other.end_bound()) {
	// 			if other.is_empty() {
	// 				None
	// 			} else {
	// 				// pick between other.start and self.end
	// 				Some(match other.start_bound() {
	// 					Bound::Included(a) => a.clone(),
	// 					Bound::Excluded(a) => a.succ().unwrap(),
	// 					Bound::Unbounded => T::MIN,
	// 				})
	// 			}
	// 		} else if self.is_empty() {
	// 			None
	// 		} else {
	// 			// pick between self.start and other.end
	// 			Some(match self.start_bound() {
	// 				Bound::Included(a) => a.clone(),
	// 				Bound::Excluded(a) => a.succ().unwrap(),
	// 				Bound::Unbounded => T::MIN,
	// 			})
	// 		}
	// 	} else {
	// 		None
	// 	}
	// }
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

impl<T> From<std::ops::Range<T>> for AnyRange<T> {
	fn from(value: std::ops::Range<T>) -> Self {
		Self {
			start: Bound::Included(value.start),
			end: Bound::Excluded(value.end),
		}
	}
}

impl<T> From<std::ops::RangeInclusive<T>> for AnyRange<T> {
	fn from(value: std::ops::RangeInclusive<T>) -> Self {
		let (start, end) = value.into_inner();

		Self {
			start: Bound::Included(start),
			end: Bound::Included(end),
		}
	}
}

impl<T> From<std::ops::RangeFrom<T>> for AnyRange<T> {
	fn from(value: std::ops::RangeFrom<T>) -> Self {
		Self {
			start: Bound::Included(value.start),
			end: Bound::Unbounded,
		}
	}
}

impl<T> From<std::ops::RangeTo<T>> for AnyRange<T> {
	fn from(value: std::ops::RangeTo<T>) -> Self {
		Self {
			start: Bound::Unbounded,
			end: Bound::Excluded(value.end),
		}
	}
}

impl<T> From<std::ops::RangeToInclusive<T>> for AnyRange<T> {
	fn from(value: std::ops::RangeToInclusive<T>) -> Self {
		Self {
			start: Bound::Unbounded,
			end: Bound::Included(value.end),
		}
	}
}

impl<T> From<std::ops::RangeFull> for AnyRange<T> {
	fn from(_: std::ops::RangeFull) -> Self {
		Self {
			start: Bound::Unbounded,
			end: Bound::Unbounded,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn intersection_test1() {
		let a: AnyRange<char> = ('A'..='A').into();
		assert!(a.intersects(&a))
	}

	#[test]
	fn intersection_test2() {
		let a: AnyRange<char> = ('A'..='A').into();
		let b: AnyRange<char> = ('B'..='B').into();
		assert!(!a.intersects(&b))
	}
}
