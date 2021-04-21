use super::Len;

pub enum Saturating<T> {
	Saturated,
	Sub(T)
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
			(Saturating::Sub(a), Saturating::Sub(b)) => {
				a.saturating_add(b)
			}
		}
	}
}

impl<T: Len> std::ops::Add<T> for Saturating<T> {
	type Output = Self;

	fn add(self, other: T) -> Self {
		match self {
			Saturating::Saturated => Self::Saturated,
			Saturating::Sub(t) => {
				t.saturating_add(other)
			}
		}
	}
}

impl<T: std::ops::Sub<Output=T>> std::ops::Sub<T> for Saturating<T> {
	type Output = Self;

	fn sub(self, other: T) -> Self {
		match self {
			Saturating::Saturated => Self::Saturated,
			Saturating::Sub(t) => {
				Saturating::Sub(t - other)
			}
		}
	}
}