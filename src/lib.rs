#![feature(bound_cloned)]

mod range;
pub mod generic;

pub use range::*;

pub type RangeMap<K, V> = generic::RangeMap<K, V, slab::Slab<btree_slab::generic::Node<AnyRange<K>, V>>>;