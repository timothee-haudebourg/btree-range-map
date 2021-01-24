#![feature(bound_cloned)]
#![feature(trait_alias)]

mod range;
pub mod generic;

pub use range::*;

pub type RangeSet<K> = generic::RangeSet<K, slab::Slab<btree_slab::generic::Node<AnyRange<K>, ()>>>;
pub type RangeMap<K, V> = generic::RangeMap<K, V, slab::Slab<btree_slab::generic::Node<AnyRange<K>, V>>>;
pub type RangeMultiMap<K, S> = generic::RangeMultiMap<K, S, slab::Slab<btree_slab::generic::Node<AnyRange<K>, S>>>;