use super::Saturating;

pub trait Len:
	Sized + Default + std::ops::Add<Output = Self> + std::ops::Sub<Output = Self>
{
	fn saturating_add(self, other: Self) -> Saturating<Self>;
}

macro_rules! impl_len {
	($ty:ident) => {
		impl Len for $ty {
			fn saturating_add(self, other: Self) -> Saturating<Self> {
				let (result, overflow) = self.overflowing_add(other);
				if overflow {
					Saturating::Saturated
				} else {
					Saturating::Sub(result)
				}
			}
		}
	};
}

impl_len!(u8);
impl_len!(u16);
impl_len!(u32);
impl_len!(u64);
impl_len!(u128);
impl_len!(usize);
impl_len!(i8);
impl_len!(i16);
impl_len!(i32);
impl_len!(i64);
impl_len!(i128);
impl_len!(isize);

impl Len for f32 {
	fn saturating_add(self, other: Self) -> Saturating<Self> {
		let sum = self + other;
		if sum.is_infinite() {
			Saturating::Saturated
		} else {
			Saturating::Sub(sum)
		}
	}
}

impl Len for f64 {
	fn saturating_add(self, other: Self) -> Saturating<Self> {
		let sum = self + other;
		if sum.is_infinite() {
			Saturating::Saturated
		} else {
			Saturating::Sub(sum)
		}
	}
}
