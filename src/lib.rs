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
		Item
	}
};

pub struct ExclusiveRange<T> {
	start: T,
	end: T
}

impl<T> ExclusiveRange<T> {
	fn contains(&self, t: &T) -> bool where T: PartialOrd {
		&self.start >= t && t < &self.end
	}

	pub fn intersects(&self, other: &Range<T>) -> bool where T: PartialOrd {
		(other.start < self.end && self.end <= other.end) || (self.start < other.end && other.end <= self.end)
	}

	pub fn connected_to(&self, other: &Range<T>) -> bool where T: PartialOrd {
		self.intersects(other) || self.start == other.end || self.end == other.start
	}

	pub fn is_empty(&self) -> bool where T: PartialOrd {
		self.end <= self.start
	}

	pub fn without(&self, other: &Range<T>) -> (Option<ExclusiveRange<T>>, Option<ExclusiveRange<T>>) where T: PartialOrd + Clone {
		let left = if self.start <= other.start {
			Some(ExclusiveRange {
				start: self.start.clone(),
				end: other.start.clone()
			})
		} else {
			None
		};

		let right = if other.end <= self.end {
			Some(ExclusiveRange {
				start: other.end.clone(),
				end: self.end.clone()
			})
		} else {
			None
		};

		(left, right)
	}
}

impl<T> From<Range<T>> for ExclusiveRange<T> {
	fn from(range: Range<T>) -> ExclusiveRange<T> {
		ExclusiveRange {
			start: range.start,
			end: range.end
		}
	}
}

impl<T: Ord> Ord for ExclusiveRange<T> {
	fn cmp(&self, other: &ExclusiveRange<T>) -> Ordering {
		// It is very important that the end bound is used for comparison here.
		// Because for a brief moment in the range insertion algorithm, exclusives ranges may not be ordered by `start`.
		self.end.cmp(&other.end)
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

impl<T: PartialEq> PartialEq for ExclusiveRange<T> {
	fn eq(&self, other: &ExclusiveRange<T>) -> bool {
		self.start.eq(&other.start) && self.end.eq(&other.end)
	}
}

impl<T: Eq> Eq for ExclusiveRange<T> { }

pub trait RangeOrd<T: Ord, U> {
	fn cmp(a: &ExclusiveRange<T>, b: &U) -> Ordering;

	fn matches(a: &ExclusiveRange<T>, b: &U) -> bool;
}

pub struct RangeMap<K, V> {
	btree: BTreeMap<ExclusiveRange<K>, V>
}

impl<K: Ord, V> RangeMap<K, V> {
	/// Create a new empty map.
	pub fn new() -> RangeMap<K, V> {
		RangeMap {
			btree: BTreeMap::new()
		}
	}

	fn address_of<T, O: RangeOrd<K, T>>(&self, key: &T) -> Result<ItemAddr, ItemAddr> {
		match self.btree.root_id() {
			Some(id) => self.address_in::<T, O>(id, key),
			None => Err(ItemAddr::nowhere())
		}
	}

	fn address_in<T, O: RangeOrd<K, T>>(&self, mut id: usize, key: &T) -> Result<ItemAddr, ItemAddr> {
		loop {
			match self.offset_in::<T, O>(id, key) {
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

	fn offset_in<T, O: RangeOrd<K, T>>(&self, id: usize, key: &T) -> Result<usize, (usize, Option<usize>)> {
		match self.btree.node(id) {
			Node::Internal(node) => {
				let branches = node.branches();
				match binary_search_with::<_, _, _, _, O>(branches, key) {
					Some(i) => {
						let b = &branches[i];
						if O::matches(b.item.key(), key) {
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
				match binary_search_with::<_, _, _, _, O>(items, key) {
					Some(i) => {
						let item = &items[i];
						if O::matches(item.key(), key) {
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

	pub fn get(&self, key: &K) -> Option<&V> {
		match self.address_of::<_, KeyOrd>(key) {
			Ok(addr) => Some(self.btree.item(addr).unwrap().value()),
			Err(_) => None
		}
	}

	/// Insert a new key-value binding.
	pub fn insert_range(&mut self, key: Range<K>, mut value: V) where K: Clone, V: PartialEq + Clone {
		match self.address_of::<_, ConnectedOrd>(&key) {
			Ok(addr) => {
				// some work to do here...
				let mut prev_addr = Some(addr);
				while let Some(mut addr) = prev_addr {
					if self.btree.item(addr).map(|item| item.key().intersects(&key)).unwrap_or(false) {
						match self.btree.item(addr).unwrap().key().without(&key) {
							(Some(left), Some(right)) => {
								let right_value = if !right.is_empty() {
									let right_value = {
										let item = self.btree.item_mut(addr).unwrap();
										*item.key_mut() = right;
										item.value().clone()
									};
									
									addr = self.btree.insert_at(addr, Item::new(key.into(), value));
									right_value
								} else {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = key.into();
									std::mem::swap(&mut value, item.value_mut());
									value
								};

								if !left.is_empty() {
									self.btree.insert_at(addr, Item::new(left, right_value));
								}

								return // no need to go further, the inserted range was totaly included in this one.
							},
							(Some(left), None) => {
								let left_value = {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = key.into();
									std::mem::swap(&mut value, item.value_mut());
									value
								};

								if !left.is_empty() {
									self.btree.insert_at(addr, Item::new(left, left_value));
								}
								
								return // no need to go further, the inserted range does not intersect anything below this range.
							},
							(None, Some(right)) => {
								if !right.is_empty() {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = right;
								} else {
									let (_, next_addr) = self.btree.remove_at(addr).unwrap();
									addr = next_addr
								}
							},
							(None, None) => {
								let (_, next_addr) = self.btree.remove_at(addr).unwrap();
								addr = next_addr
							}
						}

						prev_addr = self.btree.previous_address(addr);
					} else {
						self.btree.insert_at(addr, Item::new(key.into(), value));
						return
					}
				}
			},
			Err(addr) => {
				// there are no connected ranges. We can freely insert this new range.
				self.btree.insert_at(addr, Item::new(key.into(), value));
			}
		}
	}

	/// Remove a key.
	///
	/// Returns the value that was bound to this key, if any.
	pub fn remove_range(&mut self, key: &Range<K>) where K: Clone, V: Clone {
		match self.address_of::<_, IntersectionOrd>(key) {
			Ok(addr) => {
				let mut prev_addr = Some(addr);
				while let Some(mut addr) = prev_addr {
					if self.btree.item(addr).map(|item| item.key().intersects(key)).unwrap_or(false) {
						match self.btree.item(addr).unwrap().key().without(key) {
							(Some(left), Some(right)) => {
								let right_value = {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = right;
									item.value().clone()
								};
								self.btree.insert_at(addr, Item::new(left, right_value));
								break // no need to go further, the removed range was totaly included in this one.
							},
							(Some(left), None) => {
								let item = self.btree.item_mut(addr).unwrap();
								*item.key_mut() = left;
								break // no need to go further, the removed range does not intersect anything below this range.
							},
							(None, Some(right)) => {
								let item = self.btree.item_mut(addr).unwrap();
								*item.key_mut() = right;
							},
							(None, None) => {
								let (_, next_addr) = self.btree.remove_at(addr).unwrap();
								addr = next_addr
							}
						}

						prev_addr = self.btree.previous_address(addr);
					} else {
						break
					}
				}
			},
			Err(_) => ()
		}
	}
}

pub struct KeyOrd;

impl<T: Ord> RangeOrd<T, T> for KeyOrd {
	fn cmp(a: &ExclusiveRange<T>, b: &T) -> Ordering {
		a.start.cmp(b)
	}

	fn matches(a: &ExclusiveRange<T>, b: &T) -> bool {
		a.contains(b)
	}
}

pub struct ConnectedOrd;

impl<T: Ord> RangeOrd<T, Range<T>> for ConnectedOrd {
	fn cmp(a: &ExclusiveRange<T>, b: &Range<T>) -> Ordering {
		match a.start.cmp(&b.end) {
			Ordering::Equal => Ordering::Less,
			ordering => ordering
		}
	}

	fn matches(a: &ExclusiveRange<T>, b: &Range<T>) -> bool {
		a.connected_to(b)
	}
}

pub struct IntersectionOrd;

impl<T: Ord> RangeOrd<T, Range<T>> for IntersectionOrd {
	fn cmp(a: &ExclusiveRange<T>, b: &Range<T>) -> Ordering {
		a.start.cmp(&b.end)
	}

	fn matches(a: &ExclusiveRange<T>, b: &Range<T>) -> bool {
		a.intersects(b)
	}
}

pub fn binary_search_with<T: Ord, U, V, I: AsRef<Item<ExclusiveRange<T>, V>>, O: RangeOrd<T, U>>(items: &[I], range: &U) -> Option<usize> {
	if items.is_empty() || O::cmp(items[0].as_ref().key(), range) != Ordering::Less {
		None
	} else {
		let mut i = 0;
		let mut j = items.len() - 1;

		if O::cmp(items[j].as_ref().key(), range) == Ordering::Less {
			return Some(j)
		}

		// invariants:
		// vec[i].as_ref().key() < range
		// vec[j].as_ref().key() >= range
		// j > i

		while j-i > 1 {
			let k = (i + j) / 2;

			if O::cmp(items[k].as_ref().key(), range) != Ordering::Less {
				j = k;
			} else {
				i = k;
			}
		}

		Some(i)
	}
}