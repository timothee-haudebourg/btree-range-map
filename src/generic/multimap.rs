use crate::{
	generic::{
		map::{IntoIter, Iter},
		RangeMap,
	},
	AnyRange, AsRange,
};
use btree_slab::generic::Node;
use cc_traits::{SetMut, SimpleCollectionMut, SimpleCollectionRef, Slab, SlabMut};
use range_traits::{Measure, PartialEnum};

/// Multi map.
///
/// In a multi map, each key is associated to a set of values.
/// The type parameter `S` is the set type. It can be replaced by anything
/// implementing the [`cc_traits::SetMut`] trait, such as the standard
/// [`BTreeSet`](std::collections::BTreeSet) and [`HashSet`](std::collections::HashSet).
#[derive(Clone)]
pub struct RangeMultiMap<K, S, C> {
	map: RangeMap<K, S, C>,
}

impl<K, S, C> RangeMultiMap<K, S, C> {
	pub fn new() -> RangeMultiMap<K, S, C>
	where
		C: Default,
	{
		RangeMultiMap {
			map: RangeMap::new(),
		}
	}
}

impl<K, S, C: Default> Default for RangeMultiMap<K, S, C> {
	fn default() -> Self {
		Self::new()
	}
}

impl<K, S, C: Slab<Node<AnyRange<K>, S>>> RangeMultiMap<K, S, C>
where
	C: SimpleCollectionRef,
{
	pub fn iter(&self) -> Iter<K, S, C> {
		self.map.iter()
	}
}

impl<'a, K: Clone + PartialOrd + Measure, S, C: Slab<Node<AnyRange<K>, S>>> IntoIterator
	for &'a RangeMultiMap<K, S, C>
where
	C: SimpleCollectionRef,
{
	type Item = (&'a AnyRange<K>, &'a S);
	type IntoIter = Iter<'a, K, S, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<K, S, C: SlabMut<Node<AnyRange<K>, S>>> RangeMultiMap<K, S, C>
where
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	pub fn insert<R: AsRange<Item = K>, V>(&mut self, key: R, value: V)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq + Clone,
		S: SetMut<V> + PartialEq + Clone + Default,
	{
		self.map.update(key, |set_opt| {
			let mut result = match set_opt {
				Some(set) => set.clone(),
				None => S::default(),
			};

			result.insert(value.clone());
			Some(result)
		})
	}

	pub fn remove<R: AsRange<Item = K>, V>(&mut self, key: R, value: &V)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq + Clone,
		S: SetMut<V> + PartialEq + Clone + Default,
	{
		self.map.update(key, |set_opt| match set_opt {
			Some(set) => {
				let mut result = set.clone();
				result.remove(value);
				if result.is_empty() {
					None
				} else {
					Some(result)
				}
			}
			None => None,
		})
	}
}

impl<K: Clone + PartialOrd + Measure, S, C: SlabMut<Node<AnyRange<K>, S>>> IntoIterator
	for RangeMultiMap<K, S, C>
where
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	type Item = (AnyRange<K>, S);
	type IntoIter = IntoIter<K, S, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.map.into_iter()
	}
}
