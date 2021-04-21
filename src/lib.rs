#![feature(bound_cloned)]
#![feature(trait_alias)]

pub mod util;
mod range;
pub mod generic;

pub use range::*;

pub type DefaultSetContainer<K> = slab::Slab<btree_slab::generic::Node<AnyRange<K>, ()>>;
pub type DefaultMapContainer<K, V> = slab::Slab<btree_slab::generic::Node<AnyRange<K>, V>>;

pub type RangeSet<K> = generic::RangeSet<K, DefaultSetContainer<K>>;
pub type RangeMap<K, V> = generic::RangeMap<K, V, DefaultMapContainer<K, V>>;
pub type RangeMultiMap<K, S> = generic::RangeMultiMap<K, S, DefaultMapContainer<K, S>>;