use std::{
	ops::Range,
	cmp::{
		Ord,
		PartialOrd,
		Ordering
	}
};
use linear_btree::{
	map::{
		BTreeMap,
		BTreeExt
	},
	node::{
		ItemAddr,
		Node,
		internal::Branch
	}
};

pub struct ExclusiveRange<T> {
	start: T,
	end: T
}

impl<T> ExclusiveRange<T> {
	fn contains(&self, t: T) -> bool where T: PartialOrd {
		self.start >= t && t < self.end
	}

	fn intersects(&self, other: &Range<T>) -> bool where T: PartialOrd {
		panic!("TODO")
	}
}

impl<T: Ord> Ord for ExclusiveRange<T> {
	fn cmp(&self, other: &ExclusiveRange<T>) -> Ordering {
		self.start.cmp(&other.start)
	}
}

impl<T: Ord> PartialOrd for ExclusiveRange<T> {
	fn partial_cmp(&self, other: &ExclusiveRange<T>) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl<T: PartialEq> PartialEq<Range<T>> for ExclusiveRange<T> {
	fn eq(&self, other: &Range<T>) -> bool {
		self.start.eq(&other.start) && self.end.eq(&other.end)
	}
}

impl<T: Ord> PartialOrd<Range<T>> for ExclusiveRange<T> {
	fn partial_cmp(&self, other: &Range<T>) -> Option<Ordering> {
		Some(self.start.cmp(&other.start))
	}
}

impl<T: PartialEq> PartialEq for ExclusiveRange<T> {
	fn eq(&self, other: &ExclusiveRange<T>) -> bool {
		self.start.eq(&other.start) && self.end.eq(&other.end)
	}
}

impl<T: Eq> Eq for ExclusiveRange<T> { }

pub struct RangeMap<K, V> {
	btree: BTreeMap<ExclusiveRange<K>, V>
}

impl<K, V> RangeMap<K, V> {
	/// Create a new empty map.
	pub fn new() -> RangeMap<K, V> {
		RangeMap {
			btree: BTreeMap::new()
		}
	}

	fn address_of(&self, key: &Range<K>) -> Result<ItemAddr, ItemAddr> where K: PartialOrd {
		match self.btree.root_id() {
			Some(id) => self.address_in(id, key),
			None => Err(ItemAddr::nowhere())
		}
	}

	fn address_in(&self, id: usize, key: &Range<K>) -> Result<ItemAddr, ItemAddr> where K: PartialOrd {
		loop {
			match self.offset_in(id, key) {
				Ok(offset) => {
					return Err(ItemAddr::new(id, offset))
				},
				Err((offset, None)) => {
					return Err(ItemAddr::new(id, offset))
				},
				Err((_, Some(child_id))) => {
					id = child_id;
				}
			}
		}
	}

	fn offset_in(&self, id: usize, key: &Range<K>) -> Result<usize, (usize, Option<usize>)> where K: PartialOrd {
		match self.btree.node(id) {
			Node::Internal(node) => {
				let branches = node.branches();
				match binary_search_previous_range(branches, key) {
					Some(i) => {
						let b = &branches[i];
						if b.item.key().intersects(key) {
							Ok(i)
						} else {
							Err((i, Some(b.child)))
						}
					},
					None => {
						Err((0, Some(node.first_child_id())))
					}
				}
			},
			Node::Leaf(leaf) => {
				let items = leaf.items();
				match binary_search_previous_range(items, key) {
					Some(i) => {
						let item = &items[i];
						if item.key().intersects(key) {
							Ok(i)
						} else {
							Err((i, None))
						}
					},
					None => {
						Err((0, None))
					}
				}
			}
		}
	}

	/// Insert a new key-value binding.
	pub fn insert_range(&mut self, key: Range<K>, value: V) where K: PartialOrd {
		match self.address_of(&key) {
			Ok(addr) => {
				panic!("TODO")
			},
			Err(addr) => {
				let addr_prev = self.btree.previous_address(addr);
				let addr_next = self.btree.next_address(addr);

				match self.btree.item(addr_prev) {
					Some(prev) if prev.key().end == key.start => {
						match self.btree.item(addr_next) {
							Some(next) if key.end == next.key().start => {
								// merge prev e next
							},
							None => {
								// merge prev e
							}
						}
					},
					_ => {
						match self.btree.item(addr_next) {
							Some(next) if key.end == next.key().start => {
								// merge e next
							},
							None => {
								// just insert...
							}
						}
					}
				}

				panic!("TODO")
			}
		}
	}

	/// Remove a key.
	///
	/// Returns the value that was bound to this key, if any.
	pub fn remove_range(&mut self, key: &Range<K>) -> Option<V> {
		panic!("TODO")
	}
}

/// Search in `branches` for the item with the nearest range key before or intersecting the given one.
///
/// `branches` is assumed to be sorted.
#[inline]
pub fn binary_search_previous_range<T, V>(branches: &[Branch<ExclusiveRange<T>, V>], range: &Range<T>) -> Option<usize> where T: PartialOrd {
	if branches.is_empty() || branches[0].item.key().start >= range.end {
		None
	} else {
		let mut i = 0;
		let mut j = branches.len() - 1;

		if branches[j].item.key().start < range.end {
			return Some(j)
		}

		// invariants:
		// vec[i].item.key().start < range.end
		// vec[j].item.key().start >= range.end
		// j > i

		while j-i > 1 {
			let k = (i + j) / 2;

			if branches[k].item.key().start >= range.end {
				j = k;
			} else {
				i = k;
			}
		}

		Some(i)
	}
}
