use std::ops::{Bound, RangeBounds};

/// Range where both bounds are excluded.
pub struct RangeFromExcludedTo<T> {
	pub start: T,
	pub end: T,
}

impl<T> RangeFromExcludedTo<T> {
	pub const fn new(start: T, end: T) -> RangeFromExcludedTo<T> {
		RangeFromExcludedTo { start, end }
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
