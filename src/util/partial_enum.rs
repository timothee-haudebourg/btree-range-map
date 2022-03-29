pub trait PartialEnum: Sized {
	// const INFINITE_MIN: Self;

	const MIN: Self;
	const MAX: Self;

	fn pred(&self) -> Option<Self>;

	fn succ(&self) -> Option<Self>;
}

impl PartialEnum for char {
	// const INFINITE_MIN: char = '\u{000000}';

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
