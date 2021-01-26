use cc_traits::{
	Slab,
	SlabMut,
	SetMut
};
use btree_slab::generic::{
	Node
};
use crate::{
	Measure,
	AnyRange,
	AsRange,
	generic::{
		RangeMap,
		map::{
			Iter,
			IntoIter
		}
	}
};

#[derive(Clone)]
pub struct RangeMultiMap<K, S, C> {
	map: RangeMap<K, S, C>
}

impl<K, S, C> RangeMultiMap<K, S, C> {
	pub fn new() -> RangeMultiMap<K, S, C> where C: Default {
		RangeMultiMap {
			map: RangeMap::new()
		}
	}
}

impl<K, S, C: Slab<Node<AnyRange<K>, S>>> RangeMultiMap<K, S, C> {
	pub fn iter(&self) -> Iter<K, S, C> {
		self.map.iter()
	}
}

impl<'a, K: Clone + PartialOrd + Measure, S, C: Slab<Node<AnyRange<K>, S>>> IntoIterator for &'a RangeMultiMap<K, S, C> {
	type Item = (&'a AnyRange<K>, &'a S);
	type IntoIter = Iter<'a, K, S, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<K, S, C: SlabMut<Node<AnyRange<K>, S>>> RangeMultiMap<K, S, C> {
	pub fn insert<R: AsRange<Item=K>, V>(&mut self, key: R, value: V) where K: Clone + PartialOrd + Measure, V: PartialEq + Clone, S: SetMut<V> + PartialEq + Clone + Default {
		self.map.update(key, |set_opt| {
			let mut result = match set_opt {
				Some(set) => set.clone(),
				None => S::default()
			};

			result.insert(value.clone());
			Some(result)
		})
	}

	pub fn remove<R: AsRange<Item=K>, V>(&mut self, key: R, value: &V) where K: Clone + PartialOrd + Measure, V: PartialEq + Clone, S: SetMut<V> + PartialEq + Clone + Default {
		self.map.update(key, |set_opt| {
			match set_opt {
				Some(set) => {
					let mut result = set.clone();
					result.remove(value);
					if result.is_empty() {
						None
					} else {
						Some(result)
					}
				},
				None => None
			}
		})
	}

	pub fn into_iter(self) -> IntoIter<K, S, C> {
		self.map.into_iter()
	}
}

impl<K: Clone + PartialOrd + Measure, S, C: SlabMut<Node<AnyRange<K>, S>>> IntoIterator for RangeMultiMap<K, S, C> {
	type Item = (AnyRange<K>, S);
	type IntoIter = IntoIter<K, S, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.into_iter()
	}
}