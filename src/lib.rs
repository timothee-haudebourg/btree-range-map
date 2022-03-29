//! A *range map* is a map where keys are aggregated into ranges of keys for
//! efficient storage. Every time you need to store a large number numeric-keyed
//! items in a map or set, a range map (or range set) should be used.
//!
//! This library provides a range map implementation based on
//! [`btree-slab`](https://crates.io/crates/btree-slab)'s B-tree.
//! It defines three basic types `RangeSet<T>`, `RangeMap<K, V>` and
//! `RangeMultiMap<K, S>`.
//!
//! ## Usage
//!
//! The `RangeSet<T>` and `RangeMap<K, V>` behave similarly to the standard
//! `BTreeSet<T>` and `BTreeMap<K, V>` types.
//! However in addition to `PartialOrd`, the key type must also implement the
//! `Measure` trait defining how keys are merged into ranges.
//! This trait is implemented by default for `char`, integer types and float
//! types.
//!
//! ```
//! use btree_range_map::RangeMap;
//!
//! let mut range_map: RangeMap<i32, bool> = RangeMap::new();
//! range_map.insert(00..=05, true);
//! range_map.insert(4, false);
//! assert_eq!(range_map.range_count(), 3);
//! assert_eq!(range_map.get(03), Some(&true));
//! assert_eq!(range_map.get(04), Some(&false));
//! assert_eq!(range_map.get(05), Some(&true));
//! ```
//!
//! This library supports included and excluded bounds:
//!
//! ```
//! # use btree_range_map::RangeMap;
//! # let mut range_map: RangeMap<i32, bool> = RangeMap::new();
//! range_map.insert(..1, true);
//! range_map.insert(..=1, true);
//! range_map.insert(2, true);
//! range_map.insert(3..5, true);
//! range_map.insert(5..=7, true);
//! range_map.insert(7.., true);
//! assert_eq!(range_map.range_count(), 1);
//! ```
//!
//! It also supports non standard ranges with excluded start bounds:
//!
//! ```
//! # use btree_range_map::RangeMap;
//! # let mut range_map: RangeMap<i32, bool> = RangeMap::new();
//! use btree_range_map::{
//!   RangeFromExcluded,
//!   RangeFromExcludedTo,
//!   RangeFromExcludedToIncluded
//! };
//!
//! // same as       `4..`
//! range_map.insert(RangeFromExcluded::new(3), true);
//!
//! // same as       `3`
//! range_map.insert(RangeFromExcludedTo::new(2, 4), true);
//!
//! // same as       `1..=2`
//! range_map.insert(RangeFromExcludedToIncluded::new(0, 2), true);
//!
//! assert_eq!(range_map.range_count(), 1);
//! ```
//!
//! ### Floats
//!
//! Floating point numbers `f32` and `f64` are handled as one might expect.
//!
//! ```
//! use btree_range_map::{RangeMap, RangeFromExcluded};
//! let mut range_map: RangeMap<f32, bool> = RangeMap::new();
//!
//! // sets all `f32` below zero to `false`.
//! range_map.insert(..0.0, false);
//!
//! // sets all `f32` above zero to `true`.
//! range_map.insert(RangeFromExcluded::new(0.0), true);
//!
//! assert_eq!(range_map.range_count(), 2);
//! assert_eq!(range_map.get(0.0), None); // only `0.0` is unmapped.
//! ```
pub mod generic;
mod range;
pub mod util;

pub use range::*;

pub type DefaultSetContainer<K> = slab::Slab<generic::Node<AnyRange<K>, ()>>;
pub type DefaultMapContainer<K, V> = slab::Slab<generic::Node<AnyRange<K>, V>>;

pub type RangeSet<K> = generic::RangeSet<K, DefaultSetContainer<K>>;
pub type RangeMap<K, V> = generic::RangeMap<K, V, DefaultMapContainer<K, V>>;
pub type RangeMultiMap<K, S> = generic::RangeMultiMap<K, S, DefaultMapContainer<K, S>>;
