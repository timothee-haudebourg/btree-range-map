use crate::{AnyRange, AsRange, IntoRange, RangePartialOrd, generic::RangeMap};
use range_traits::{Bounded, Measure, PartialEnum};
use raw_btree::{Item, Storage};
use std::{
	cmp::Ordering,
	fmt,
	hash::{Hash, Hasher},
};

/// Range set.
///
/// This is based on a range map, where the values are `()`.
pub struct RangeSet<T, C: Storage<Item<AnyRange<T>, ()>>> {
	map: RangeMap<T, (), C>,
}

impl<T: Clone, C: Storage<Item<AnyRange<T>, ()>>> Clone for RangeSet<T, C> {
	fn clone(&self) -> Self {
		RangeSet {
			map: self.map.clone(),
		}
	}
}

impl<T, C: Storage<Item<AnyRange<T>, ()>>> RangeSet<T, C> {
	pub fn new() -> RangeSet<T, C> {
		RangeSet {
			map: RangeMap::new(),
		}
	}
}

impl<T, C: Storage<Item<AnyRange<T>, ()>>> Default for RangeSet<T, C> {
	fn default() -> Self {
		Self::new()
	}
}

impl<T, C: Storage<Item<AnyRange<T>, ()>>> RangeSet<T, C> {
	pub fn range_count(&self) -> usize {
		self.map.range_count()
	}

	pub fn len(&self) -> T::Len
	where
		T: Measure + PartialEnum + Bounded,
	{
		self.map.len()
	}

	pub fn bounded_len(&self) -> Option<T::Len>
	where
		T: Measure + PartialEnum,
	{
		self.map.bounded_len()
	}

	pub fn is_empty(&self) -> bool
	where
		T: Measure + PartialEnum,
	{
		self.map.is_empty()
	}

	pub fn intersects<R: AsRange<Item = T>>(&self, values: R) -> bool
	where
		T: Clone + PartialEnum + Measure,
	{
		self.map.intersects(values)
	}

	/// # Complexity
	///
	/// `O(log n)`, where `n` is [`Self::range_count`]. Delegates to
	/// [`RangeMap::contains_key`].
	pub fn contains(&self, value: T) -> bool
	where
		T: Clone + PartialEnum + RangePartialOrd + Measure,
	{
		self.map.contains_key(value)
	}

	pub fn iter(&self) -> Iter<'_, T, C> {
		Iter {
			inner: self.map.iter(),
		}
	}

	/// Returns an iterator over the gaps (missing values) of the set.
	pub fn gaps(&self) -> Gaps<'_, T, C> {
		self.map.gaps()
	}
}

impl<T: fmt::Debug, C: Storage<Item<AnyRange<T>, ()>>> fmt::Debug for RangeSet<T, C> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{")?;

		for range in self {
			write!(f, "{:?}", range)?
		}

		write!(f, "}}")
	}
}

impl<'a, T, C: Storage<Item<AnyRange<T>, ()>>> IntoIterator for &'a RangeSet<T, C> {
	type Item = &'a AnyRange<T>;
	type IntoIter = Iter<'a, T, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<T, C: Storage<Item<AnyRange<T>, ()>>> RangeSet<T, C> {
	/// # Complexity
	///
	/// `O((k + 1) log n)`, where `n` is [`Self::range_count`] and `k` is the
	/// number of existing ranges that overlap, or are connected to, `key`.
	/// Delegates to [`RangeMap::insert`].
	pub fn insert<R: IntoRange<Item = T>>(&mut self, key: R)
	where
		T: Clone + PartialEnum + Measure,
	{
		self.map.insert(key, ())
	}

	/// # Complexity
	///
	/// `O((k + 1) log n)`, where `n` is [`Self::range_count`] and `k` is the
	/// number of existing ranges that intersect `key`. Delegates to
	/// [`RangeMap::remove`].
	pub fn remove<R: AsRange<Item = T>>(&mut self, key: R)
	where
		T: Clone + PartialEnum + Measure,
	{
		self.map.remove(key)
	}

	pub fn complement(&self) -> Self
	where
		T: Clone + Measure + PartialEnum,
	{
		self.gaps().map(AnyRange::cloned).collect()
	}
}

impl<K, C, D> PartialEq<RangeSet<K, D>> for RangeSet<K, C>
where
	K: Measure + PartialOrd + PartialEnum,
	C: Storage<Item<AnyRange<K>, ()>>,
	D: Storage<Item<AnyRange<K>, ()>>,
{
	fn eq(&self, other: &RangeSet<K, D>) -> bool {
		self.map == other.map
	}
}

impl<K, C: Storage<Item<AnyRange<K>, ()>>> Eq for RangeSet<K, C> where K: Measure + PartialEnum + Ord
{}

impl<K, C, D> PartialOrd<RangeSet<K, D>> for RangeSet<K, C>
where
	K: Measure + PartialOrd + PartialEnum,
	C: Storage<Item<AnyRange<K>, ()>>,
	D: Storage<Item<AnyRange<K>, ()>>,
{
	fn partial_cmp(&self, other: &RangeSet<K, D>) -> Option<Ordering> {
		self.map.partial_cmp(&other.map)
	}
}

impl<K, C: Storage<Item<AnyRange<K>, ()>>> Ord for RangeSet<K, C>
where
	K: Measure + PartialEnum + Ord,
{
	fn cmp(&self, other: &Self) -> Ordering {
		self.map.cmp(&other.map)
	}
}

impl<K, C: Storage<Item<AnyRange<K>, ()>>> Hash for RangeSet<K, C>
where
	K: Hash + PartialEnum,
{
	fn hash<H: Hasher>(&self, h: &mut H) {
		self.map.hash(h)
	}
}

impl<T, C: Storage<Item<AnyRange<T>, ()>>> IntoIterator for RangeSet<T, C> {
	type Item = AnyRange<T>;
	type IntoIter = IntoIter<T, C>;

	fn into_iter(self) -> Self::IntoIter {
		IntoIter {
			inner: self.map.into_iter(),
		}
	}
}

pub struct Iter<'a, T, C: Storage<Item<AnyRange<T>, ()>>> {
	inner: crate::generic::map::Iter<'a, T, (), C>,
}

impl<'a, T, C: Storage<Item<AnyRange<T>, ()>>> Iterator for Iter<'a, T, C> {
	type Item = &'a AnyRange<T>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.inner.next() {
			Some((range, ())) => Some(range),
			None => None,
		}
	}
}

pub struct IntoIter<T, C: Storage<Item<AnyRange<T>, ()>>> {
	inner: crate::generic::map::IntoIter<T, (), C>,
}

impl<T, C: Storage<Item<AnyRange<T>, ()>>> Iterator for IntoIter<T, C> {
	type Item = AnyRange<T>;

	fn next(&mut self) -> Option<Self::Item> {
		self.inner.next().map(|(range, _)| range)
	}
}

/// Iterator over the gaps (unbound keys) of a `RangeSet`.
pub type Gaps<'a, T, C> = crate::generic::map::Gaps<'a, T, (), C>;

impl<R: IntoRange, C: Storage<Item<AnyRange<R::Item>, ()>>> std::iter::Extend<R>
	for RangeSet<R::Item, C>
where
	R::Item: Clone + Measure + PartialOrd,
{
	fn extend<I: IntoIterator<Item = R>>(&mut self, iter: I) {
		for range in iter {
			self.insert(range)
		}
	}
}

impl<R: IntoRange, C: Storage<Item<AnyRange<R::Item>, ()>>> FromIterator<R> for RangeSet<R::Item, C>
where
	R::Item: Clone + Measure + PartialOrd,
{
	fn from_iter<I: IntoIterator<Item = R>>(iter: I) -> Self {
		let mut result = Self::default();
		result.extend(iter);
		result
	}
}

#[cfg(test)]
mod test {
	use crate::{AnyRange, RangeSet};

	#[test]
	fn gaps1() {
		let mut a: RangeSet<u8> = RangeSet::new();
		let mut b: RangeSet<u8> = RangeSet::new();

		a.insert(10..20);

		b.insert(0..10);
		b.insert(20..);

		assert_eq!(a.complement(), b)
	}

	#[test]
	fn gaps2() {
		let mut a: RangeSet<u8> = RangeSet::new();
		let mut b: RangeSet<u8> = RangeSet::new();

		a.insert(0..10);
		b.insert(10..);

		assert_eq!(a.complement(), b)
	}

	#[test]
	fn gaps3() {
		let mut a: RangeSet<u8> = RangeSet::new();
		let mut b: RangeSet<u8> = RangeSet::new();

		a.insert(20..);
		b.insert(0..=19);

		assert_eq!(a.complement(), b)
	}

	#[test]
	fn gaps4() {
		let mut a: RangeSet<u8> = RangeSet::new();

		a.insert(10..20);

		let mut gaps = a.gaps().map(AnyRange::cloned);
		assert_eq!(gaps.next(), Some(AnyRange::from(..10u8)));
		assert_eq!(gaps.next(), Some(AnyRange::from(20u8..)));
		assert_eq!(gaps.next(), None)
	}

	#[test]
	fn gaps5() {
		let mut a: RangeSet<u8> = RangeSet::new();

		a.insert(..10);
		a.insert(20..);

		let mut gaps = a.gaps().map(AnyRange::cloned);
		assert_eq!(gaps.next(), Some(AnyRange::from(10..20)));
		assert_eq!(gaps.next(), None)
	}

	#[test]
	fn gaps6() {
		let mut a: RangeSet<u8> = RangeSet::new();

		a.insert(10..20);

		assert_eq!(a.complement().complement(), a)
	}
}
