use crate::{
	generic::RangeMap,
	util::{Measure, PartialEnum},
	AnyRange, AsRange,
};
use btree_slab::generic::Node;
use cc_traits::{Slab, SlabMut};
use std::{
	cmp::Ordering,
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
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<T>, ()>>,
{
	pub fn len(&self) -> T::Len
	where
		T: Measure,
	{
		self.map.len()
	}

	pub fn iter(&self) -> Iter<T, C> {
		Iter {
			inner: self.map.iter(),
		}
	}
}

impl<'a, T, C: Slab<Node<AnyRange<T>, ()>>> IntoIterator for &'a RangeSet<T, C>
where
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<T>, ()>>,
{
	type Item = &'a AnyRange<T>;
	type IntoIter = Iter<'a, T, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> RangeSet<T, C>
where
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<T>, ()>>,
	for<'r> C::ItemMut<'r>: Into<&'r mut Node<AnyRange<T>, ()>>,
{
	pub fn insert<R: AsRange<Item = T>>(&mut self, key: R)
	where
		T: Clone + PartialOrd + Measure,
	{
		self.map.insert(key, ())
	}

	pub fn remove<R: AsRange<Item = T>>(&mut self, key: R)
	where
		T: Clone + PartialOrd + Measure,
	{
		self.map.remove(key)
	}
}

impl<K, L, C: Slab<Node<AnyRange<K>, ()>>, D: Slab<Node<AnyRange<L>, ()>>> PartialEq<RangeSet<L, D>>
	for RangeSet<K, C>
where
	L: Measure<K> + PartialOrd<K>,
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<K>, ()>>,
	for<'r> D::ItemRef<'r>: Into<&'r Node<AnyRange<L>, ()>>,
{
	fn eq(&self, other: &RangeSet<L, D>) -> bool {
		self.map == other.map
	}
}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Eq for RangeSet<K, C>
where
	K: Measure + Ord,
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<K>, ()>>,
{
}

impl<K, L, C: Slab<Node<AnyRange<K>, ()>>, D: Slab<Node<AnyRange<L>, ()>>>
	PartialOrd<RangeSet<L, D>> for RangeSet<K, C>
where
	L: Measure<K> + PartialOrd<K>,
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<K>, ()>>,
	for<'r> D::ItemRef<'r>: Into<&'r Node<AnyRange<L>, ()>>,
{
	fn partial_cmp(&self, other: &RangeSet<L, D>) -> Option<Ordering> {
		self.map.partial_cmp(&other.map)
	}
}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Ord for RangeSet<K, C>
where
	K: Measure + Ord,
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<K>, ()>>,
{
	fn cmp(&self, other: &Self) -> Ordering {
		self.map.cmp(&other.map)
	}
}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Hash for RangeSet<K, C>
where
	K: Hash + PartialEnum,
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<K>, ()>>,
{
	fn hash<H: Hasher>(&self, h: &mut H) {
		self.map.hash(h)
	}
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> IntoIterator for RangeSet<T, C>
where
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<T>, ()>>,
	for<'r> C::ItemMut<'r>: Into<&'r mut Node<AnyRange<T>, ()>>,
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
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<T>, ()>>,
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
	for<'r> C::ItemRef<'r>: Into<&'r Node<AnyRange<T>, ()>>,
	for<'r> C::ItemMut<'r>: Into<&'r mut Node<AnyRange<T>, ()>>,
{
	type Item = AnyRange<T>;

	fn next(&mut self) -> Option<Self::Item> {
		self.inner.next().map(|(range, _)| range)
	}
}
