use std::{
	ops::Bound,
	cmp::Ordering
};
use super::{
	BoundOrdering,
	BoundPartialOrd,
	Measure,
	direct_bound_partial_cmp
};

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

#[inline(always)]
pub(crate) fn invert_bound<T>(bound: Bound<T>) -> Option<Bound<T>> {
	match bound {
		Bound::Unbounded => None,
		Bound::Included(t) => Some(Bound::Excluded(t)),
		Bound::Excluded(t) => Some(Bound::Included(t))
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

pub(crate) fn min_bound<'a, T: Measure + PartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>, start: bool) -> Bound<&'a T> {
	match direct_bound_partial_cmp(a, b, start) {
		Some(BoundOrdering::Included(_)) => a,
		_ => b
	}
}

pub(crate) fn max_bound<'a, T: Measure + PartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>, start: bool) -> Bound<&'a T> {
	match direct_bound_partial_cmp(a, b, start) {
		Some(BoundOrdering::Included(_)) => b,
		_ => a
	}
}