use super::{Len, PartialEnum, Saturating};

/// Distance between singletons.
#[allow(clippy::len_without_is_empty)]
pub trait Measure<U = Self>: PartialEnum {
	type Len: Len;

	fn len(&self) -> Self::Len;

	fn distance(&self, other: Saturating<&U>) -> Saturating<Self::Len>;
}

// impl<'a, U, T: Measure<U>> Measure<U> for &'a T {
// 	type Len = T::Len;

// 	fn len(&self) -> Self::Len {
// 		(*self).len()
// 	}
// }

impl Measure for char {
	type Len = u32;

	fn len(&self) -> u32 {
		1
	}

	fn distance(&self, other: Saturating<&char>) -> Saturating<u32> {
		match other {
			Saturating::Saturated => {
				if *self as u32 == 0 {
					Saturating::Saturated
				} else {
					Saturating::Sub(u32::MAX - *self as u32 + 1)
				}
			}
			Saturating::Sub(other) => {
				let a = *self as u32;
				let b = *other as u32;

				if a > b {
					Saturating::Sub(a - b)
				} else {
					Saturating::Sub(b - a)
				}
			}
		}
	}
}

macro_rules! impl_measure {
	(@refl $ty:ty, $cast: ty, $len:ty) => {
		impl PartialEnum for $ty {
			const INFINITE_MIN: $ty = <$ty>::MIN;
			const MIN: $ty = <$ty>::MIN;
			const MAX: $ty = <$ty>::MAX;

			fn pred(&self) -> Option<Self> {
				self.checked_sub(1)
			}

			fn succ(&self) -> Option<Self> {
				self.checked_add(1)
			}
		}

		impl_measure!($ty, $ty, $cast, $len);
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

			fn distance(&self, other: Saturating<&$ty2>) -> Saturating<$len> {
				match other {
					Saturating::Saturated => {
						if *self == Self::MIN {
							Saturating::Saturated
						} else {
							Saturating::Sub((Self::MAX as $cast - *self as $cast) as $len + 1)
						}
					}
					Saturating::Sub(other) => {
						let a = *self as $cast;
						let b = *other as $cast;

						if a > b {
							Saturating::Sub((a - b) as $len)
						} else {
							Saturating::Sub((b - a) as $len)
						}
					}
				}
			}
		}
	};
}

impl_measure!(@refl u8, u8, u8);
impl_measure!(@both u8, u16, u16, u16);
impl_measure!(@both u8, u32, u32, u32);
impl_measure!(@both u8, u64, u64, u64);
impl_measure!(@both u8, i8, i16, u16);
impl_measure!(@both u8, i16, i16, u16);
impl_measure!(@both u8, i32, i32, u32);
impl_measure!(@both u8, i64, i64, u64);

impl_measure!(@refl u16, u16, u16);
impl_measure!(@both u16, u32, u32, u32);
impl_measure!(@both u16, u64, u64, u64);
impl_measure!(@both u16, i8, i32, u32);
impl_measure!(@both u16, i16, i32, u32);
impl_measure!(@both u16, i32, i64, u32);
impl_measure!(@both u16, i64, i128, u64);

impl_measure!(@refl u32, u32, u32);
impl_measure!(@both u32, u64, u64, u64);
impl_measure!(@both u32, i8, i64, u64);
impl_measure!(@both u32, i16, i64, u64);
impl_measure!(@both u32, i32, i64, u64);
impl_measure!(@both u32, i64, i128, u64);

impl_measure!(@refl u64, u64, u64);
impl_measure!(@both u64, i8, i128, u128);
impl_measure!(@both u64, i16, i128, u128);
impl_measure!(@both u64, i32, i128, u128);
impl_measure!(@both u64, i64, i128, u128);

impl_measure!(@refl i8, i16, u8);
impl_measure!(@both i8, i16, i32, u16);
impl_measure!(@both i8, i32, i64, u32);
impl_measure!(@both i8, i64, i128, u64);

impl_measure!(@refl i16, i32, u16);
impl_measure!(@both i16, i32, i64, u32);
impl_measure!(@both i16, i64, i128, u64);

impl_measure!(@refl i32, i64, u32);
impl_measure!(@both i32, i64, i128, u64);

impl_measure!(@refl i64, i128, u64);

impl_measure!(@refl usize, usize, usize);

macro_rules! impl_f_measure {
	(@refl $ty:ty, $len:ty) => {
		impl PartialEnum for $ty {
			const INFINITE_MIN: $ty = <$ty>::NEG_INFINITY;
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

			fn distance(&self, other: Saturating<&$ty2>) -> Saturating<$len> {
				if self.is_infinite() {
					Saturating::Saturated
				} else {
					match other {
						Saturating::Saturated => Saturating::Saturated,
						Saturating::Sub(other) => {
							let a = *self as $cast;
							let b = *other as $cast;

							if a > b {
								Saturating::Sub((a - b) as $len)
							} else {
								Saturating::Sub((b - a) as $len)
							}
						}
					}
				}
			}
		}
	};
}

impl_f_measure!(@refl f32, f32);
impl_f_measure!(@refl f64, f64);
