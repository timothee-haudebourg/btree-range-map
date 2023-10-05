use super::{AsBound, AsRange, Directed, Measure};
use range_traits::{MaybeBounded, PartialEnum};
use std::{
	cmp::{Ordering, PartialOrd},
	ops::Bound,
};

#[derive(Debug)]
pub enum RangeOrdering {
	Before(bool),
	Intersecting(bool, bool),
	After(bool),
}

impl RangeOrdering {
	pub fn is_before(&self, connected: bool) -> bool {
		match self {
			RangeOrdering::Before(c) => !*c || !connected,
			_ => false,
		}
	}

	pub fn is_after(&self, connected: bool) -> bool {
		match self {
			RangeOrdering::After(c) => !*c || !connected,
			_ => false,
		}
	}

	pub fn matches(&self, connected: bool) -> bool {
		match self {
			RangeOrdering::Before(c) => *c && connected,
			RangeOrdering::After(c) => *c && connected,
			RangeOrdering::Intersecting(_, _) => true,
		}
	}
}

pub trait RangePartialOrd<T = Self> {
	fn range_partial_cmp<R: AsRange<Item = T>>(&self, other: &R) -> Option<RangeOrdering>;
}

impl<R: AsRange, U> RangePartialOrd<U> for R
where
	R::Item: PartialOrd<U> + Measure<U>,
	U: PartialEnum,
{
	fn range_partial_cmp<S: AsRange<Item = U>>(&self, other: &S) -> Option<RangeOrdering> {
		match direct_bound_partial_cmp(self.start(), other.start(), true) {
			Some(BoundOrdering::Included(limit_before)) => {
				match inverse_bound_partial_cmp(self.start(), other.end(), false) {
					Some(BoundOrdering::Included(_)) => {
						match direct_bound_partial_cmp(self.end(), other.end(), false) {
							Some(BoundOrdering::Included(limit_after)) => {
								Some(RangeOrdering::Intersecting(limit_before, limit_after))
							}
							Some(BoundOrdering::Excluded(_)) => {
								Some(RangeOrdering::Intersecting(limit_before, false))
							}
							None => None,
						}
					}
					Some(BoundOrdering::Excluded(limit_after)) => {
						Some(RangeOrdering::After(limit_after))
					}
					None => None,
				}
			}
			Some(BoundOrdering::Excluded(_)) => {
				match inverse_bound_partial_cmp(self.end(), other.start(), true) {
					Some(BoundOrdering::Included(_)) => {
						match direct_bound_partial_cmp(self.end(), other.end(), false) {
							Some(BoundOrdering::Included(limit_after)) => {
								Some(RangeOrdering::Intersecting(false, limit_after))
							}
							Some(BoundOrdering::Excluded(_)) => {
								Some(RangeOrdering::Intersecting(false, false))
							}
							None => None,
						}
					}
					Some(BoundOrdering::Excluded(limit_before)) => {
						Some(RangeOrdering::Before(limit_before))
					}
					None => None,
				}
			}
			None => None,
		}
	}
}

pub enum BoundOrdering {
	Included(bool),
	Excluded(bool),
}

pub trait BoundPartialOrd<T = Self> {
	fn bound_partial_cmp<B: AsBound<Item = T>>(&self, other: &Directed<B>)
		-> Option<BoundOrdering>;
}

impl<B: AsBound, U> BoundPartialOrd<U> for Directed<B>
where
	B::Item: PartialOrd<U> + Measure<U> + PartialEnum,
	U: PartialEnum,
{
	fn bound_partial_cmp<C: AsBound<Item = U>>(
		&self,
		other: &Directed<C>,
	) -> Option<BoundOrdering> {
		match (self, other) {
			(Directed::Start(a), Directed::Start(b)) => {
				direct_bound_partial_cmp(a.bound(), b.bound(), true)
			}
			(Directed::Start(a), Directed::End(b)) => {
				inverse_bound_partial_cmp(a.bound(), b.bound(), false)
			}
			(Directed::End(a), Directed::Start(b)) => {
				inverse_bound_partial_cmp(a.bound(), b.bound(), true)
			}
			(Directed::End(a), Directed::End(b)) => {
				direct_bound_partial_cmp(a.bound(), b.bound(), false)
			}
		}
	}
}

pub trait BoundOrd<T = Self> {
	fn bound_cmp<B: AsBound<Item = T>>(&self, other: &Directed<B>) -> BoundOrdering;
}

impl<B: AsBound> BoundOrd<B::Item> for Directed<B>
where
	B::Item: Ord + Measure + PartialEnum,
{
	fn bound_cmp<C: AsBound<Item = B::Item>>(&self, other: &Directed<C>) -> BoundOrdering {
		match (self, other) {
			(Directed::Start(a), Directed::Start(b)) => {
				direct_bound_cmp(a.bound(), b.bound(), true)
			}
			(Directed::Start(a), Directed::End(b)) => {
				inverse_bound_cmp(a.bound(), b.bound(), false)
			}
			(Directed::End(a), Directed::Start(b)) => inverse_bound_cmp(a.bound(), b.bound(), true),
			(Directed::End(a), Directed::End(b)) => direct_bound_cmp(a.bound(), b.bound(), false),
		}
	}
}

pub enum Dist {
	Equals,
	Zero,
	One,
	More,
}

fn dist<T, U>(t: &T, u: &U) -> Dist
where
	T: PartialEnum + PartialEq<U>,
{
	if t == u {
		return Dist::Equals;
	}

	if let Some(s) = t.succ() {
		if s == *u {
			return Dist::Zero;
		} else {
			match s.succ() {
				Some(ss) if ss == *u => return Dist::One,
				_ => (),
			}
		}
	}

	if let Some(s) = t.pred() {
		if s == *u {
			return Dist::Zero;
		} else {
			match s.pred() {
				Some(ss) if ss == *u => return Dist::One,
				_ => (),
			}
		}
	}

	Dist::More
}

fn distance_zero<T, U>(t: &T, u: &U) -> bool
where
	T: PartialEnum + PartialEq<U>,
{
	match t.succ() {
		Some(s) if s == *u => return true,
		_ => (),
	}

	match t.pred() {
		Some(p) if p == *u => return true,
		_ => (),
	}

	false
}

pub(crate) fn direct_bound_partial_cmp<T, U>(
	b1: Bound<&T>,
	b2: Bound<&U>,
	start: bool,
) -> Option<BoundOrdering>
where
	T: Measure<U> + PartialOrd<U> + PartialEnum,
	U: PartialEnum,
{
	let included_ord = if start {
		Ordering::Greater
	} else {
		Ordering::Less
	};

	match (b1, b2) {
		(Bound::Included(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => Some(BoundOrdering::Excluded(distance_zero(v1, v2))),
			None => None,
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
			Some(ord) if ord == included_ord => {
				Some(BoundOrdering::Included(distance_zero(v1, v2)))
			}
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None,
		},
		(Bound::Included(v1), Bound::Unbounded) => Some(BoundOrdering::Included(
			(start && U::min().map(|m| *v1 == m).unwrap_or(false))
				|| (!start && U::max().map(|m| *v1 == m).unwrap_or(false)),
		)),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(false)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => match dist(v1, v2) {
				Dist::Zero => Some(BoundOrdering::Included(true)),
				Dist::One => Some(BoundOrdering::Excluded(true)),
				_ => Some(BoundOrdering::Excluded(false)),
			},
			None => None,
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => Some(BoundOrdering::Excluded(distance_zero(v1, v2))),
			None => None,
		},
		(Bound::Excluded(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Unbounded, Bound::Included(v2)) => {
			if (start && U::min().map(|m| *v2 == m).unwrap_or(false))
				|| (!start && U::max().map(|m| *v2 == m).unwrap_or(false))
			{
				Some(BoundOrdering::Included(true))
			} else {
				Some(BoundOrdering::Excluded(
					(start
						&& v2
							.pred()
							.and_then(|pred| U::min().map(|m| pred == m))
							.unwrap_or(false)) || (!start
						&& v2
							.succ()
							.and_then(|succ| U::min().map(|m| succ == m))
							.unwrap_or(false)),
				))
			}
		}
		(Bound::Unbounded, Bound::Excluded(_)) => Some(BoundOrdering::Excluded(false)),
		(Bound::Unbounded, Bound::Unbounded) => Some(BoundOrdering::Included(true)),
	}
}

pub(crate) fn direct_bound_cmp<T>(b1: Bound<&T>, b2: Bound<&T>, start: bool) -> BoundOrdering
where
	T: Measure + PartialEnum + Ord,
{
	let included_ord = if start {
		Ordering::Greater
	} else {
		Ordering::Less
	};

	match (b1, b2) {
		(Bound::Included(v1), Bound::Included(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Included(true),
			ord if ord == included_ord => BoundOrdering::Included(false),
			_ => BoundOrdering::Excluded(distance_zero(v1, v2)),
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Excluded(true),
			ord if ord == included_ord => BoundOrdering::Included(distance_zero(v1, v2)),
			_ => BoundOrdering::Excluded(false),
		},
		(Bound::Included(v1), Bound::Unbounded) => BoundOrdering::Included(
			(start && Some(v1) == <T as MaybeBounded>::min().as_ref())
				|| (!start && Some(v1) == <T as MaybeBounded>::max().as_ref()),
		),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Included(false),
			ord if ord == included_ord => BoundOrdering::Included(false),
			_ => match dist(v1, v2) {
				Dist::Zero => BoundOrdering::Included(true),
				Dist::One => BoundOrdering::Excluded(true),
				_ => BoundOrdering::Excluded(false),
			},
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Included(true),
			ord if ord == included_ord => BoundOrdering::Included(false),
			_ => BoundOrdering::Excluded(distance_zero(v1, v2)),
		},
		(Bound::Excluded(_), Bound::Unbounded) => BoundOrdering::Included(false),
		(Bound::Unbounded, Bound::Included(v2)) => {
			if (start && <T as MaybeBounded>::min().as_ref() == Some(v2))
				|| (!start && <T as MaybeBounded>::max().as_ref() == Some(v2))
			{
				BoundOrdering::Included(true)
			} else {
				BoundOrdering::Excluded(
					(start
						&& v2
							.pred()
							.map(|pred| <T as MaybeBounded>::min() == Some(pred))
							.unwrap_or(false)) || (!start
						&& v2
							.succ()
							.map(|succ| <T as MaybeBounded>::min() == Some(succ))
							.unwrap_or(false)),
				)
			}
		}
		(Bound::Unbounded, Bound::Excluded(_)) => BoundOrdering::Excluded(false),
		(Bound::Unbounded, Bound::Unbounded) => BoundOrdering::Included(true),
	}
}

pub(crate) fn direct_bound_partial_eq<T, U>(b1: Bound<&T>, b2: Bound<&U>, start: bool) -> bool
where
	T: Measure<U> + PartialOrd<U> + PartialEnum,
	U: PartialEnum,
{
	match direct_bound_partial_cmp(b1, b2, start) {
		Some(BoundOrdering::Included(eq)) => eq,
		_ => false,
	}
}

// pub(crate) fn direct_bound_eq<T>(b1: Bound<&T>, b2: Bound<&T>, start: bool) -> bool where T: Measure + Ord {
// 	match direct_bound_cmp(b1, b2, start) {
// 		BoundOrdering::Included(eq) => eq,
// 		_ => false
// 	}
// }

pub(crate) fn inverse_bound_partial_cmp<T, U>(
	b1: Bound<&T>,
	b2: Bound<&U>,
	b2_start: bool,
) -> Option<BoundOrdering>
where
	T: Measure<U> + PartialOrd<U> + PartialEnum,
	U: PartialEnum,
{
	let included_ord = if b2_start {
		Ordering::Greater
	} else {
		Ordering::Less
	};

	match (b1, b2) {
		(Bound::Included(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => Some(BoundOrdering::Excluded(distance_zero(v1, v2))),
			None => None,
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
			Some(ord) if ord == included_ord => {
				Some(BoundOrdering::Included(distance_zero(v1, v2)))
			}
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None,
		},
		(Bound::Included(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)), // []v2=v1
			Some(ord) if ord == included_ord => {
				Some(BoundOrdering::Included(distance_zero(v1, v2)))
			}
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None,
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(false)),
			Some(ord) if ord == included_ord => match dist(v1, v2) {
				Dist::Zero => Some(BoundOrdering::Excluded(true)), // v2 [] v1
				Dist::One => Some(BoundOrdering::Included(true)),  // v2 [ x ] v1
				_ => Some(BoundOrdering::Included(false)),         // v2 [ x .. y ] v1
			},
			Some(_) => Some(BoundOrdering::Excluded(false)), // ] v1 v2 [
			None => None,
		},
		(Bound::Excluded(v1), Bound::Unbounded) => {
			if (!b2_start && U::max().map(|m| *v1 == m).unwrap_or(false))
				|| (b2_start && U::min().map(|m| *v1 == m).unwrap_or(false))
			{
				Some(BoundOrdering::Excluded(true))
			} else {
				Some(BoundOrdering::Included(
					(!b2_start
						&& v1
							.pred()
							.map(|pred| U::min().map(|m| pred == m).unwrap_or(false))
							.unwrap_or(false)) || (b2_start
						&& v1
							.succ()
							.map(|succ| U::max().map(|m| succ == m).unwrap_or(false))
							.unwrap_or(false)),
				))
			}
		}
		(Bound::Unbounded, Bound::Included(_)) => Some(BoundOrdering::Included(false)),
		(Bound::Unbounded, Bound::Excluded(v2)) => {
			if (!b2_start && U::min().map(|m| *v2 == m).unwrap_or(false))
				|| (b2_start && U::max().map(|m| *v2 == m).unwrap_or(false))
			{
				Some(BoundOrdering::Excluded(true))
			} else {
				Some(BoundOrdering::Included(
					(!b2_start
						&& v2
							.pred()
							.map(|pred| U::min().map(|m| pred == m).unwrap_or(false))
							.unwrap_or(false)) || (b2_start
						&& v2
							.succ()
							.map(|succ| U::max().map(|m| succ == m).unwrap_or(false))
							.unwrap_or(false)),
				))
			}
		}
		(Bound::Unbounded, Bound::Unbounded) => Some(BoundOrdering::Included(false)),
	}
}

pub(crate) fn inverse_bound_cmp<T>(b1: Bound<&T>, b2: Bound<&T>, b2_start: bool) -> BoundOrdering
where
	T: Measure + Ord + PartialEnum,
{
	let included_ord = if b2_start {
		Ordering::Greater
	} else {
		Ordering::Less
	};

	match (b1, b2) {
		(Bound::Included(v1), Bound::Included(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Included(true),
			ord if ord == included_ord => BoundOrdering::Included(false),
			_ => BoundOrdering::Excluded(distance_zero(v1, v2)),
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Excluded(true),
			ord if ord == included_ord => BoundOrdering::Included(distance_zero(v1, v2)),
			_ => BoundOrdering::Excluded(false),
		},
		(Bound::Included(_), Bound::Unbounded) => BoundOrdering::Included(false),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Excluded(true), // []v2=v1
			ord if ord == included_ord => BoundOrdering::Included(distance_zero(v1, v2)),
			_ => BoundOrdering::Excluded(false),
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.cmp(v2) {
			Ordering::Equal => BoundOrdering::Excluded(false),
			ord if ord == included_ord => match dist(v1, v2) {
				Dist::Zero => BoundOrdering::Excluded(true), // v2 [] v1
				Dist::One => BoundOrdering::Included(true),  // v2 [ x ] v1
				_ => BoundOrdering::Included(false),         // v2 [ x .. y ] v1
			},
			_ => BoundOrdering::Excluded(false), // ] v1 v2 [
		},
		(Bound::Excluded(v1), Bound::Unbounded) => {
			if (!b2_start
				&& <T as MaybeBounded>::max()
					.map(|m| *v1 == m)
					.unwrap_or(false))
				|| (b2_start
					&& <T as MaybeBounded>::min()
						.map(|m| *v1 == m)
						.unwrap_or(false))
			{
				BoundOrdering::Excluded(true)
			} else {
				BoundOrdering::Included(
					(!b2_start
						&& v1
							.pred()
							.map(|pred| {
								<T as MaybeBounded>::min()
									.map(|m| pred == m)
									.unwrap_or(false)
							})
							.unwrap_or(false)) || (b2_start
						&& v1
							.succ()
							.map(|succ| {
								<T as MaybeBounded>::max()
									.map(|m| succ == m)
									.unwrap_or(false)
							})
							.unwrap_or(false)),
				)
			}
		}
		(Bound::Unbounded, Bound::Included(_)) => BoundOrdering::Included(false),
		(Bound::Unbounded, Bound::Excluded(v2)) => {
			if (!b2_start
				&& <T as MaybeBounded>::min()
					.map(|m| *v2 == m)
					.unwrap_or(false))
				|| (b2_start
					&& <T as MaybeBounded>::max()
						.map(|m| *v2 == m)
						.unwrap_or(false))
			{
				BoundOrdering::Excluded(true)
			} else {
				BoundOrdering::Included(
					(!b2_start
						&& v2
							.pred()
							.map(|pred| {
								<T as MaybeBounded>::min()
									.map(|m| pred == m)
									.unwrap_or(false)
							})
							.unwrap_or(false)) || (b2_start
						&& v2
							.succ()
							.map(|succ| {
								<T as MaybeBounded>::max()
									.map(|m| succ == m)
									.unwrap_or(false)
							})
							.unwrap_or(false)),
				)
			}
		}
		(Bound::Unbounded, Bound::Unbounded) => BoundOrdering::Included(false),
	}
}

// fn inverse_bound_partial_eq<T, U>(b1: Bound<&T>, b2: Bound<&U>, start: bool) -> bool where T: Measure<U> + PartialOrd<U> {
// 	match inverse_bound_partial_cmp(b1, b2, start) {
// 		Some(BoundOrdering::Included(eq)) => eq,
// 		_ => false
// 	}
// }
