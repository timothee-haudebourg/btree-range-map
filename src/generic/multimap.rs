use crate::{
	AnyRange, AsRange,
	generic::{
		RangeMap,
		map::{IntoIter, Iter},
	},
};
use cc_traits::SetMut;
use range_traits::{Measure, PartialEnum};
use raw_btree::{Item, Storage};

/// Multi map.
///
/// In a multi map, each key is associated to a set of values.
/// The type parameter `S` is the set type. It can be replaced by anything
/// implementing the [`cc_traits::SetMut`] trait, such as the standard
/// [`BTreeSet`](std::collections::BTreeSet) and [`HashSet`](std::collections::HashSet).
pub struct RangeMultiMap<K, S, C: Storage<Item<AnyRange<K>, S>>> {
	map: RangeMap<K, S, C>,
}

impl<K: Clone, S: Clone, C: Storage<Item<AnyRange<K>, S>>> Clone for RangeMultiMap<K, S, C> {
	fn clone(&self) -> Self {
		RangeMultiMap {
			map: self.map.clone(),
		}
	}
}

impl<K, S, C: Storage<Item<AnyRange<K>, S>>> RangeMultiMap<K, S, C> {
	pub fn new() -> RangeMultiMap<K, S, C> {
		RangeMultiMap {
			map: RangeMap::new(),
		}
	}
}

impl<K, S, C: Storage<Item<AnyRange<K>, S>>> Default for RangeMultiMap<K, S, C> {
	fn default() -> Self {
		Self::new()
	}
}

impl<K, S, C: Storage<Item<AnyRange<K>, S>>> RangeMultiMap<K, S, C> {
	pub fn iter(&self) -> Iter<'_, K, S, C> {
		self.map.iter()
	}
}

impl<'a, K: Clone + PartialOrd + Measure, S, C: Storage<Item<AnyRange<K>, S>>> IntoIterator
	for &'a RangeMultiMap<K, S, C>
{
	type Item = (&'a AnyRange<K>, &'a S);
	type IntoIter = Iter<'a, K, S, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<K, S, C: Storage<Item<AnyRange<K>, S>>> RangeMultiMap<K, S, C> {
	/// # Complexity
	///
	/// `O((k + 1) log n + |S|)`, where `n` is the number of ranges in the map,
	/// `k` is the number of existing ranges that overlap, or are connected
	/// to, `key` (see [`RangeMap::update`]), and `|S|` is the size of the
	/// value set attached to the affected range, which is cloned and mutated
	/// by this operation.
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

	/// # Complexity
	///
	/// `O((k + 1) log n + |S|)`, where `n` is the number of ranges in the map,
	/// `k` is the number of existing ranges that overlap, or are connected
	/// to, `key` (see [`RangeMap::update`]), and `|S|` is the size of the
	/// value set attached to the affected range, which is cloned and mutated
	/// by this operation.
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

impl<K: Clone + PartialOrd + Measure, S, C: Storage<Item<AnyRange<K>, S>>> IntoIterator
	for RangeMultiMap<K, S, C>
{
	type Item = (AnyRange<K>, S);
	type IntoIter = IntoIter<K, S, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.map.into_iter()
	}
}
