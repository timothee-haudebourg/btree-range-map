use cc_traits::{
	Slab,
	SlabMut
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

pub struct RangeSet<T, C: Slab<Node<AnyRange<T>, ()>>> {
	map: RangeMap<T, (), C>
}

impl<T, C: Slab<Node<AnyRange<T>, ()>>> RangeSet<T, C> {
	pub fn len(&self) -> T::Len where T: Measure {
		self.map.len()
	}
}

impl<T, C: SlabMut<Node<AnyRange<T>, ()>>> RangeSet<T, C> {
	pub fn insert<R: AsRange<Item=T>>(&mut self, key: R) where T: Clone + PartialOrd + Measure {
		self.map.insert(key, ())
	}
}