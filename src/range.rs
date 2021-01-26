use std::{
	fmt::Debug,
	ops::{
		RangeBounds,
		Bound
	},
	cmp::{
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

	fn is_empty(&self) -> bool where T: PartialOrd + Measure {
		is_range_empty(self.start_bound(), self.end_bound())
	}

	pub fn len(&self) -> T::Len where T: Measure {
		match (self.start_bound(), self.end_bound()) {
			(Bound::Included(a), Bound::Included(b)) => a.distance(b) + b.len(),
			(Bound::Included(a), Bound::Excluded(b)) => a.distance(b),
			(Bound::Included(a), Bound::Unbounded) => a.distance(&T::MAX),
			(Bound::Excluded(a), Bound::Included(b)) => a.distance(b) - a.len() + b.len(),
			(Bound::Excluded(a), Bound::Excluded(b)) => a.distance(b) - a.len(),
			(Bound::Excluded(a), Bound::Unbounded) => a.distance(&T::MAX) - a.len(),
			(Bound::Unbounded, Bound::Included(b)) => T::MIN.distance(b) + b.len(),
			(Bound::Unbounded, Bound::Excluded(b)) => T::MIN.distance(b),
			(Bound::Unbounded, Bound::Unbounded) => T::MIN.distance(&T::MAX)
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
}

impl<T, U> PartialEq<AnyRange<U>> for AnyRange<T> where T: Measure<U> + PartialOrd<U> {
	fn eq(&self, other: &AnyRange<U>) -> bool {
		direct_bound_partial_eq(self.start_bound(), other.start_bound(), true) &&
		direct_bound_partial_eq(self.end_bound(), other.end_bound(), false)
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

	fn intersected_with<'a, R: AsRange<Item=Self::Item>>(&'a self, other: &'a R) -> AnyRange<&'a Self::Item> where Self::Item: PartialOrd + Measure {
		AnyRange {
			start: min_bound(self.start(), other.start(), true),
			end: min_bound(self.end(), other.end(), false)
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
					end: self.end()
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
// singleton_range!(u128);
// singleton_range!(i128);
// singleton_range!(usize);
// singleton_range!(isize);
singleton_range!(f32);
singleton_range!(f64);
singleton_range!(char);

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

// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// pub enum Distance {
// 	/// Both elements are equals.
// 	Equals,

// 	/// Both elements are different, but there is no element between them.
// 	Zero,

// 	/// Both elements are different, and there is only one other elements between them.
// 	One, 

// 	/// Both elements are different, and there are multiples elements between them.
// 	More
// }

pub trait PartialEnum: Sized {
	const MIN: Self;
	const MAX: Self;

	fn pred(&self) -> Option<Self>;

	fn succ(&self) -> Option<Self>;
}

// /// Distance between singletons.
pub trait Measure<U = Self>: PartialEnum {
	type Len: Sized + Default + std::ops::Add<Output=Self::Len> + std::ops::Sub<Output=Self::Len>;

	fn len(&self) -> Self::Len;

	fn distance(&self, other: &U) -> Self::Len;
}

impl PartialEnum for char {
	const MIN: char = '\u{000000}';
	const MAX: char = '\u{10ffff}';

	fn pred(&self) -> Option<Self> {
		let c = *self as u32;
		if c == 0 || c == 0xe000 {
			None
		} else {
			Some(unsafe { std::char::from_u32_unchecked(c - 1) })
		}
	}

	fn succ(&self) -> Option<Self> {
		let c = *self as u32;
		if c == 0xd7ff || c == 0x10ffff {
			None
		} else {
			Some(unsafe { std::char::from_u32_unchecked(c + 1) })
		}
	}
}

impl Measure for char {
	type Len = u64;

	fn len(&self) -> u64 {
		1
	}

	fn distance(&self, other: &char) -> u64 {
		let a = *self as u64;
		let b = *other as u64;

		if a > b {
			a - b
		} else {
			b - a
		}
	}
}

macro_rules! impl_measure {
	(@refl $ty:ty, $len:ty) => {
		impl PartialEnum for $ty {
			const MIN: $ty = <$ty>::MIN;
			const MAX: $ty = <$ty>::MAX;

			fn pred(&self) -> Option<Self> {
				self.checked_sub(1)
			}

			fn succ(&self) -> Option<Self> {
				self.checked_add(1)
			}
		}

		impl_measure!($ty, $ty, $ty, $len);
	};
	(@both $ty1:ty, $ty2:ty, $cast:ty, $len:ty) => {
		impl_measure!($ty1, $ty2, $cast, $len);
		impl_measure!($ty2, $ty1, $cast, $len);
	};
	($ty1:ty, $ty2:ty, $cast:ty, $len:ty) => {
		impl Measure<$ty2> for $ty1 {
			type Len = $len;

			fn len(&self) -> $len {
				1
			}

			fn distance(&self, other: &$ty2) -> $len {
				let a = *self as $len;
				let b = *other as $len;

				if a > b {
					a - b
				} else {
					b - a
				}
			}
		}
	}
}

impl_measure!(@refl u8, u16);
impl_measure!(@both u8, u16, u16, u32);
impl_measure!(@both u8, u32, u32, u64);
impl_measure!(@both u8, u64, u64, u128);
impl_measure!(@both u8, i8, i16, i16);
impl_measure!(@both u8, i16, i16, i32);
impl_measure!(@both u8, i32, i32, i64);
impl_measure!(@both u8, i64, i64, i128);

impl_measure!(@refl u16, u32);
impl_measure!(@both u16, u32, u32, u64);
impl_measure!(@both u16, u64, u64, u128);
impl_measure!(@both u16, i8, i32, i32);
impl_measure!(@both u16, i16, i32, i32);
impl_measure!(@both u16, i32, i32, i64);
impl_measure!(@both u16, i64, i64, i128);

impl_measure!(@refl u32, u64);
impl_measure!(@both u32, u64, u64, u128);
impl_measure!(@both u32, i8, i64, i64);
impl_measure!(@both u32, i16, i64, i64);
impl_measure!(@both u32, i32, i64, i64);
impl_measure!(@both u32, i64, i64, i128);

impl_measure!(@refl u64, u128);
impl_measure!(@both u64, i8, i128, i128);
impl_measure!(@both u64, i16, i128, i128);
impl_measure!(@both u64, i32, i128, i128);
impl_measure!(@both u64, i64, i128, i128);

impl_measure!(@refl i8, i16);
impl_measure!(@both i8, i16, i16, i32);
impl_measure!(@both i8, i32, i32, i64);
impl_measure!(@both i8, i64, i64, i128);

impl_measure!(@refl i16, i32);
impl_measure!(@both i16, i32, i32, i64);
impl_measure!(@both i16, i64, i64, i128);

impl_measure!(@refl i32, i64);
impl_measure!(@both i32, i64, i64, i128);

impl_measure!(@refl i64, i128);

// TODO arch specific impls (for usize/isize).

macro_rules! impl_f_measure {
	(@refl $ty:ty, $len:ty) => {
		impl PartialEnum for $ty {
			const MIN: $ty = <$ty>::MIN;
			const MAX: $ty = <$ty>::MAX;

			fn pred(&self) -> Option<Self> {
				None
			}

			fn succ(&self) -> Option<Self> {
				None
			}
		}

		impl_f_measure!($ty, $ty, $ty, $len);
	};
	(@both $ty1:ty, $ty2:ty, $cast:ty, $len:ty) => {
		impl_f_measure!($ty1, $ty2, $cast, $len);
		impl_f_measure!($ty2, $ty1, $cast, $len);
	};
	($ty1:ty, $ty2:ty, $cast:ty, $len:ty) => {
		impl Measure<$ty2> for $ty1 {
			type Len = $len;

			fn len(&self) -> $len {
				0.0
			}

			fn distance(&self, other: &$ty2) -> $len {
				let a = *self as $len;
				let b = *other as $len;

				if a > b {
					a - b
				} else {
					b - a
				}
			}
		}
	}
}

impl_f_measure!(@refl f32, f32);
impl_f_measure!(@refl f64, f64);

#[derive(Debug)]
pub enum RangeOrdering {
	Before(bool),
	Intersecting(bool, bool),
	After(bool)
}

impl RangeOrdering {
	pub fn is_before(&self, connected: bool) -> bool {
		match self {
			RangeOrdering::Before(c) => !*c || !connected,
			_ => false
		}
	}

	pub fn is_after(&self, connected: bool) -> bool {
		match self {
			RangeOrdering::After(c) => !*c || !connected,
			_ => false
		}
	}

	pub fn matches(&self, connected: bool) -> bool {
		match self {
			RangeOrdering::Before(c) => *c && connected,
			RangeOrdering::After(c) => *c && connected,
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

fn min_bound<'a, T: Measure + PartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>, start: bool) -> Bound<&'a T> {
	match direct_bound_partial_cmp(a, b, start) {
		Some(BoundOrdering::Included(_)) => a,
		_ => b
	}
}

fn max_bound<'a, T: Measure + PartialOrd>(a: Bound<&'a T>, b: Bound<&'a T>, start: bool) -> Bound<&'a T> {
	match direct_bound_partial_cmp(a, b, start) {
		Some(BoundOrdering::Included(_)) => b,
		_ => a
	}
}

pub enum Dist {
	Equals,
	Zero,
	One,
	More
}

fn dist<T, U>(t: &T, u: &U) -> Dist where T: PartialEnum + PartialEq<U> {
	if t == u {
		return Dist::Equals
	}

	match t.succ() {
		Some(s) => {
			if s == *u {
				return Dist::Zero
			} else {
				match s.succ() {
					Some(ss) if ss == *u => return Dist::One,
					_ => ()
				}
			}
		},
		_ => ()
	}

	match t.pred() {
		Some(s) => {
			if s == *u {
				return Dist::Zero
			} else {
				match s.pred() {
					Some(ss) if ss == *u => return Dist::One,
					_ => ()
				}
			}
		},
		_ => ()
	}

	Dist::More
}

fn distance_zero<T, U>(t: &T, u: &U) -> bool where T: PartialEnum + PartialEq<U> {
	match t.succ() {
		Some(s) if s == *u => return true,
		_ => ()
	}

	match t.pred() {
		Some(p) if p == *u => return true,
		_ => ()
	}

	false
}

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
			Some(_) => Some(BoundOrdering::Excluded(distance_zero(v1, v2))),
			None => None
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(distance_zero(v1, v2))),
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None
		},
		(Bound::Included(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(false)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => match dist(v1, v2) {
				Dist::Zero => Some(BoundOrdering::Included(true)),
				Dist::One => Some(BoundOrdering::Excluded(true)),
				_ => Some(BoundOrdering::Excluded(false))
			},
			None => None
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Included(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(false)),
			Some(_) => Some(BoundOrdering::Excluded(distance_zero(v1, v2))),
			None => None
		},
		(Bound::Excluded(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Unbounded, Bound::Included(_)) => Some(BoundOrdering::Excluded(false)),
		(Bound::Unbounded, Bound::Excluded(_)) => Some(BoundOrdering::Excluded(false)),
		(Bound::Unbounded, Bound::Unbounded) => Some(BoundOrdering::Included(true))
	}
}

fn direct_bound_partial_eq<T, U>(b1: Bound<&T>, b2: Bound<&U>, start: bool) -> bool where T: Measure<U> + PartialOrd<U> {
	match direct_bound_partial_cmp(b1, b2, start) {
		Some(BoundOrdering::Included(eq)) => eq,
		_ => false
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
			Some(_) => Some(BoundOrdering::Excluded(distance_zero(v1, v2))),
			None => None
		},
		(Bound::Included(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)),
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(distance_zero(v1, v2))),
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None
		},
		(Bound::Included(_), Bound::Unbounded) => Some(BoundOrdering::Included(false)),
		(Bound::Excluded(v1), Bound::Included(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(true)), // []v2=v1
			Some(ord) if ord == included_ord => Some(BoundOrdering::Included(distance_zero(v1, v2))),
			Some(_) => Some(BoundOrdering::Excluded(false)),
			None => None
		},
		(Bound::Excluded(v1), Bound::Excluded(v2)) => match v1.partial_cmp(v2) {
			Some(Ordering::Equal) => Some(BoundOrdering::Excluded(false)),
			Some(ord) if ord == included_ord => match dist(v1, v2) {
				Dist::Zero => Some(BoundOrdering::Excluded(true)), // v2 [] v1
				Dist::One => Some(BoundOrdering::Included(true)), // v2 [ x ] v1
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

// fn inverse_bound_partial_eq<T, U>(b1: Bound<&T>, b2: Bound<&U>, start: bool) -> bool where T: Measure<U> + PartialOrd<U> {
// 	match inverse_bound_partial_cmp(b1, b2, start) {
// 		Some(BoundOrdering::Included(eq)) => eq,
// 		_ => false
// 	}
// }

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