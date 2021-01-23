use std::marker::PhantomData;
use cc_traits::{
	Slab,
	Set
};
use btree_slab::generic::{
	Node
};
use crate::{
	Measure,
	AnyRange,
	AsRange,
	generic::RangeMap
};

pub struct RangeMultiMap<K, S, C: Slab<Node<AnyRange<K>, S>>> {
	map: RangeMap<K, S, C>
}

impl<K: Clone + PartialOrd + Measure, S, C: Slab<Node<AnyRange<K>, S>>> RangeMultiMap<K, S, C> {
	pub fn insert<R: AsRange<Item=K>, V>(&mut self, key: R, mut value: V) where K: PartialOrd + Measure, V: PartialEq + Clone {
		// ...
	}
}