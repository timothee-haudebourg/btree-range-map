# B-Tree range map

[![CI](https://github.com/timothee-haudebourg/btree-range-map/workflows/CI/badge.svg)](https://github.com/timothee-haudebourg/btree-range-map/actions)
[![Crate informations](https://img.shields.io/crates/v/btree-range-map.svg?style=flat-square)](https://crates.io/crates/btree-range-map)
[![License](https://img.shields.io/crates/l/btree-range-map.svg?style=flat-square)](https://github.com/timothee-haudebourg/btree-range-map#license)
[![Documentation](https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square)](https://docs.rs/btree-range-map)

A *range map* is a map where keys are aggregated into ranges of keys for
efficient storage. Every time you need to store a large number numeric-keyed
items in a map or set, a range map (or range set) should be used.

This library provides a range map implementation based on
[`btree-slab`](https://crates.io/crates/btree-slab)'s B-tree.
It defines three basic types `RangeSet<T>`, `RangeMap<K, V>` and
`RangeMultiMap<K, S>`.

### Usage

The `RangeSet<T>` and `RangeMap<K, V>` behave similarly to the standard
`BTreeSet<T>` and `BTreeMap<K, V>` types.
However in addition to `PartialOrd`, the key type must also implement the
`Measure` trait defining how keys are merged into ranges.
This trait is implemented by default for `char`, integer types and float
types.

```rust
use btree_range_map::RangeMap;

let mut range_map: RangeMap<i32, bool> = RangeMap::new();
range_map.insert(00..=05, true);
range_map.insert(4, false);
assert_eq!(range_map.range_count(), 3);
assert_eq!(range_map.get(03), Some(&true));
assert_eq!(range_map.get(04), Some(&false));
assert_eq!(range_map.get(05), Some(&true));
```

This library supports included and excluded bounds:

```rust
range_map.insert(..1, true);
range_map.insert(..=1, true);
range_map.insert(2, true);
range_map.insert(3..5, true);
range_map.insert(5..=7, true);
range_map.insert(7.., true);
assert_eq!(range_map.range_count(), 1);
```

It also supports non standard ranges with excluded start bounds:

```rust
use btree_range_map::{
  RangeFromExcluded,
  RangeFromExcludedTo,
  RangeFromExcludedToIncluded
};

// same as       `4..`
range_map.insert(RangeFromExcluded::new(3), true);

// same as       `3`
range_map.insert(RangeFromExcludedTo::new(2, 4), true);

// same as       `1..=2`
range_map.insert(RangeFromExcludedToIncluded::new(0, 2), true);

assert_eq!(range_map.range_count(), 1);
```

#### Floats

Floating point numbers `f32` and `f64` are handled as one might expect.

```rust
use btree_range_map::{RangeMap, RangeFromExcluded};
let mut range_map: RangeMap<f32, bool> = RangeMap::new();

// sets all `f32` below zero to `false`.
range_map.insert(..0.0, false);

// sets all `f32` above zero to `true`.
range_map.insert(RangeFromExcluded::new(0.0), true);

assert_eq!(range_map.range_count(), 2);
assert_eq!(range_map.get(0.0), None); // only `0.0` is unmapped.
```

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
