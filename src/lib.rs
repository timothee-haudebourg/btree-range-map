#![feature(trait_alias)]

pub mod generic;
mod range;
pub mod util;

pub use range::*;

pub type DefaultSetContainer<K> = slab::Slab<generic::Node<AnyRange<K>, ()>>;
pub type DefaultMapContainer<K, V> = slab::Slab<generic::Node<AnyRange<K>, V>>;

pub type RangeSet<K> = generic::RangeSet<K, DefaultSetContainer<K>>;
pub type RangeMap<K, V> = generic::RangeMap<K, V, DefaultMapContainer<K, V>>;
pub type RangeMultiMap<K, S> = generic::RangeMultiMap<K, S, DefaultMapContainer<K, S>>;
