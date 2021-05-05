use std::hash::{
	Hash,
	Hasher
};
use cc_traits::{
	Slab,
	SlabMut
};
use btree_slab::generic::{
	Node
};
use crate::{
	util::{
		Measure,
		Saturating,
		PartialEnum
	},
	AnyRange,
	AsRange,
	generic::RangeMap
};

#[derive(Clone)]
pub struct RangeSet<T, C> {
	map: RangeMap<T, (), C>
}

impl<T, C> RangeSet<T, C> {
	pub fn new() -> RangeSet<T, C> where C: Default {
		RangeSet {
			map: RangeMap::new()
		}
	}
}

impl<T, C: Default> Default for RangeSet<T, C> {
	fn default() -> Self {
		Self::new()
	}
}

impl<T, C: Slab<Node<AnyRange<T>, ()>>> RangeSet<T, C> {
	pub fn len(&self) -> Saturating<T::Len> where T: Measure {
		self.map.len()
	}

	pub fn iter(&self) -> Iter<T, C> {
		Iter {
			inner: self.map.iter()
		}
	}
}

impl<'a, T, C: Slab<Node<AnyRange<T>, ()>>> IntoIterator for &'a RangeSet<T, C> {
	type Item = &'a AnyRange<T>;
	type IntoIter = Iter<'a, T, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> RangeSet<T, C> {
	pub fn insert<R: AsRange<Item=T>>(&mut self, key: R) where T: Clone + PartialOrd + Measure {
		self.map.insert(key, ())
	}

	pub fn remove<R: AsRange<Item=T>>(&mut self, key: R) where T: Clone + PartialOrd + Measure {
		self.map.remove(key)
	}

	pub fn into_iter(self) -> IntoIter<T, C> {
		IntoIter {
			inner: self.map.into_iter()
		}
	}
}

impl<K, L, C: Slab<Node<AnyRange<K>, ()>>, D: Slab<Node<AnyRange<L>, ()>>> PartialEq<RangeSet<L, D>> for RangeSet<K, C> where L: Measure<K> + PartialOrd<K> {
	fn eq(&self, other: &RangeSet<L, D>) -> bool {
		self.map == other.map
	}
}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Eq for RangeSet<K, C> where K: Measure + Ord {}

impl<K, C: Slab<Node<AnyRange<K>, ()>>> Hash for RangeSet<K, C> where K: Hash + PartialEnum {
	fn hash<H: Hasher>(&self, h: &mut H) {
		self.map.hash(h)
	}
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> IntoIterator for RangeSet<T, C> {
	type Item = AnyRange<T>;
	type IntoIter = IntoIter<T, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.into_iter()
	}
}

pub struct Iter<'a, T, C> {
	inner: crate::generic::map::Iter<'a, T, (), C>
}

impl<'a, T, C: Slab<Node<AnyRange<T>, ()>>> Iterator for Iter<'a, T, C> {
	type Item = &'a AnyRange<T>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.inner.next() {
			Some((range, ())) => Some(range),
			None => None
		}
	}
}

pub struct IntoIter<T, C> {
	inner: crate::generic::map::IntoIter<T, (), C>
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> Iterator for IntoIter<T, C> {
	type Item = AnyRange<T>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.inner.next() {
			Some((range, ())) => Some(range),
			None => None
		}
	}
}