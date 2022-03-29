use std::ops::{Bound, RangeBounds};

/// Range with an excluded start bound and included end bound.
pub struct RangeFromExcludedToIncluded<T> {
	pub start: T,
	pub end: T,
}

impl<T> RangeFromExcludedToIncluded<T> {
	pub const fn new(start: T, end: T) -> RangeFromExcludedToIncluded<T> {
		RangeFromExcludedToIncluded { start, end }
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
