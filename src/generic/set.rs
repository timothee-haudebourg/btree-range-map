use crate::{generic::RangeMap, AnyRange, AsRange, IntoRange, RangePartialOrd};
use btree_slab::generic::Node;
use cc_traits::{SimpleCollectionMut, SimpleCollectionRef, Slab, SlabMut};
use range_traits::{Bounded, Measure, PartialEnum};
use std::{
	cmp::Ordering,
	fmt,
	hash::{Hash, Hasher},
};

/// Range set.
///
/// This is based on a range map, where the values are `()`.
#[derive(Clone)]
pub struct RangeSet<T, C> {
	map: RangeMap<T, (), C>,
}

impl<T, C> RangeSet<T, C> {
	pub fn new() -> RangeSet<T, C>
	where
		C: Default,
	{
		RangeSet {
			map: RangeMap::new(),
		}
	}
}

impl<T, C: Default> Default for RangeSet<T, C> {
	fn default() -> Self {
		Self::new()
	}
}

impl<T, C: Slab<Node<AnyRange<T>, ()>>> RangeSet<T, C>
where
	C: SimpleCollectionRef,
{
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

	pub fn contains(&self, value: T) -> bool
	where
		T: Clone + PartialEnum + RangePartialOrd + Measure,
	{
		self.map.contains_key(value)
	}

	pub fn iter(&self) -> Iter<T, C> {
		Iter {
			inner: self.map.iter(),
		}
	}

	/// Returns an iterator over the gaps (missing values) of the set.
	pub fn gaps(&self) -> Gaps<T, C> {
		self.map.gaps()
	}
}

impl<T: fmt::Debug, C: Slab<Node<AnyRange<T>, ()>>> fmt::Debug for RangeSet<T, C>
where
	C: SimpleCollectionRef,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{")?;

		for range in self {
			write!(f, "{:?}", range)?
		}

		write!(f, "}}")
	}
}

impl<'a, T, C: Slab<Node<AnyRange<T>, ()>>> IntoIterator for &'a RangeSet<T, C>
where
	C: SimpleCollectionRef,
{
	type Item = &'a AnyRange<T>;
	type IntoIter = Iter<'a, T, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> RangeSet<T, C>
where
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	pub fn insert<R: IntoRange<Item = T>>(&mut self, key: R)
	where
		T: Clone + PartialEnum + Measure,
	{
		self.map.insert(key, ())
	}

	pub fn remove<R: AsRange<Item = T>>(&mut self, key: R)
	where
		T: Clone + PartialEnum + Measure,
	{
		self.map.remove(key)
	}

	pub fn complement(&self) -> Self
	where
		T: Clone + Measure + PartialEnum,
		C: Default,
	{
		self.gaps().map(AnyRange::cloned).collect()
	}
}

impl<K, L, C: Slab<Node<AnyRange<K>, ()>>, D: Slab<Node<AnyRange<L>, ()>>> PartialEq<RangeSet<L, D>>
	for RangeSet<K, C>
where
	L: Measure<K> + PartialOrd<K> + PartialEnum,
	K: PartialEnum,
	C: SimpleCollectionRef,
	D: SimpleCollectionRef,
{
	fn eq(&self, other: &RangeSet<L, D>) -> bool {
		self.map == other.map
	}
}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Eq for RangeSet<K, C>
where
	K: Measure + PartialEnum + Ord,
	C: SimpleCollectionRef,
{
}

impl<K, L, C: Slab<Node<AnyRange<K>, ()>>, D: Slab<Node<AnyRange<L>, ()>>>
	PartialOrd<RangeSet<L, D>> for RangeSet<K, C>
where
	L: Measure<K> + PartialOrd<K> + PartialEnum,
	K: PartialEnum,
	C: SimpleCollectionRef,
	D: SimpleCollectionRef,
{
	fn partial_cmp(&self, other: &RangeSet<L, D>) -> Option<Ordering> {
		self.map.partial_cmp(&other.map)
	}
}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Ord for RangeSet<K, C>
where
	K: Measure + PartialEnum + Ord,
	C: SimpleCollectionRef,
{
	fn cmp(&self, other: &Self) -> Ordering {
		self.map.cmp(&other.map)
	}
}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Hash for RangeSet<K, C>
where
	K: Hash + PartialEnum,
	C: SimpleCollectionRef,
{
	fn hash<H: Hasher>(&self, h: &mut H) {
		self.map.hash(h)
	}
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> IntoIterator for RangeSet<T, C>
where
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	type Item = AnyRange<T>;
	type IntoIter = IntoIter<T, C>;

	fn into_iter(self) -> Self::IntoIter {
		IntoIter {
			inner: self.map.into_iter(),
		}
	}
}

pub struct Iter<'a, T, C> {
	inner: crate::generic::map::Iter<'a, T, (), C>,
}

impl<'a, T, C: Slab<Node<AnyRange<T>, ()>>> Iterator for Iter<'a, T, C>
where
	C: SimpleCollectionRef,
{
	type Item = &'a AnyRange<T>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.inner.next() {
			Some((range, ())) => Some(range),
			None => None,
		}
	}
}

pub struct IntoIter<T, C> {
	inner: crate::generic::map::IntoIter<T, (), C>,
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> Iterator for IntoIter<T, C>
where
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	type Item = AnyRange<T>;

	fn next(&mut self) -> Option<Self::Item> {
		self.inner.next().map(|(range, _)| range)
	}
}

/// Iterator over the gaps (unbound keys) of a `RangeSet`.
pub type Gaps<'a, T, C> = crate::generic::map::Gaps<'a, T, (), C>;

impl<R: IntoRange, C: SlabMut<Node<AnyRange<R::Item>, ()>>> std::iter::Extend<R>
	for RangeSet<R::Item, C>
where
	R::Item: Clone + Measure + PartialOrd,
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	fn extend<I: IntoIterator<Item = R>>(&mut self, iter: I) {
		for range in iter {
			self.insert(range)
		}
	}
}

impl<R: IntoRange, C: Default + SlabMut<Node<AnyRange<R::Item>, ()>>> FromIterator<R>
	for RangeSet<R::Item, C>
where
	R::Item: Clone + Measure + PartialOrd,
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
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
