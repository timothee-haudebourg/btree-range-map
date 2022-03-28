use super::Len;

#[derive(Clone, Copy)]
pub enum Saturating<T> {
	Saturated,
	Sub(T),
}

impl<T: Default> Default for Saturating<T> {
	fn default() -> Self {
		Self::Sub(T::default())
	}
}

impl<T: Len> std::ops::Add for Saturating<T> {
	type Output = Self;

	fn add(self, other: Self) -> Self {
		match (self, other) {
			(Saturating::Saturated, _) => Self::Saturated,
			(_, Saturating::Saturated) => Self::Saturated,
			(Saturating::Sub(a), Saturating::Sub(b)) => a.saturating_add(b),
		}
	}
}

impl<T: Len> std::ops::Add<T> for Saturating<T> {
	type Output = Self;

	fn add(self, other: T) -> Self {
		match self {
			Saturating::Saturated => Self::Saturated,
			Saturating::Sub(t) => t.saturating_add(other),
		}
	}
}

impl<T: std::ops::Sub<Output = T>> std::ops::Sub<T> for Saturating<T> {
	type Output = Self;

	fn sub(self, other: T) -> Self {
		match self {
			Saturating::Saturated => Self::Saturated,
			Saturating::Sub(t) => Saturating::Sub(t - other),
		}
	}
}

macro_rules! saturating_cast {
	($from:ident => $to:ident) => {
		impl From<Saturating<$from>> for $to {
			fn from(v: Saturating<$from>) -> $to {
				match v {
					Saturating::Saturated => $from::MAX as $to + 1,
					Saturating::Sub(v) => v as $to,
				}
			}
		}

		impl PartialEq<$to> for Saturating<$from> {
			fn eq(&self, other: &$to) -> bool {
				std::convert::Into::<$to>::into(*self) == *other
			}
		}

		impl PartialEq<Saturating<$from>> for $to {
			fn eq(&self, other: &Saturating<$from>) -> bool {
				*other == std::convert::Into::<$to>::into(*self)
			}
		}
	};
}

saturating_cast!(u8 => u16);
saturating_cast!(u8 => u32);
saturating_cast!(u8 => u64);
saturating_cast!(u8 => usize);
saturating_cast!(u8 => u128);
saturating_cast!(u16 => u32);
saturating_cast!(u16 => u64);
saturating_cast!(u16 => usize);
saturating_cast!(u16 => u128);
saturating_cast!(u32 => u64);
saturating_cast!(u32 => u128);
saturating_cast!(u32 => usize);
saturating_cast!(u64 => u128);
