use std::{
	fmt::Debug,
	cmp::Ordering,
	ops::RangeBounds
};
use linear_btree::{
	map::{
		BTreeMap,
		BTreeExt,
		BTreeExtMut
	},
	node::{
		ItemAddr,
		Node,
		Item
	}
};
use crate::{
	Range,
	RangeOrd,
	RangeExt,
	BoundPartialOrd
};

pub struct RangeMap<K, V> {
	btree: BTreeMap<Range<K>, V>
}

impl<K: Clone + Ord + Debug, V> RangeMap<K, V> {
	/// Create a new empty map.
	pub fn new() -> RangeMap<K, V> {
		RangeMap {
			btree: BTreeMap::new()
		}
	}

	pub fn range_count(&self) -> usize {
		self.btree.len()
	}

	fn address_of<T>(&self, key: &T, disconnected: bool) -> Result<ItemAddr, ItemAddr> where K: RangeOrd<T> {
		match self.btree.root_id() {
			Some(id) => self.address_in(id, key, disconnected),
			None => Err(ItemAddr::nowhere())
		}
	}

	fn address_in<T>(&self, mut id: usize, key: &T, disconnected: bool) -> Result<ItemAddr, ItemAddr> where K: RangeOrd<T> {
		loop {
			match self.offset_in(id, key, disconnected) {
				Ok(offset) => {
					return Ok(ItemAddr::new(id, offset))
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

	fn offset_in<T>(&self, id: usize, key: &T, disconnected: bool) -> Result<usize, (usize, Option<usize>)> where K: RangeOrd<T> {
		match self.btree.node(id) {
			Node::Internal(node) => {
				let branches = node.branches();
				match binary_search_with(branches, key, disconnected) {
					Some(i) => {
						let b = &branches[i];
						if K::range_cmp(b.item.key(), key).matches(disconnected) {
							Ok(i)
						} else {
							Err((i+1, Some(b.child)))
						}
					},
					None => {
						Err((0, Some(node.first_child_id())))
					}
				}
			},
			Node::Leaf(leaf) => {
				let items = leaf.items();
				match binary_search_with(items, key, disconnected) {
					Some(i) => {
						let item = &items[i];
						println!("leaf found");
						if K::range_cmp(item.key(), key).matches(disconnected) {
							Ok(i)
						} else {
							Err((i+1, None))
						}
					},
					None => {
						println!("not found in leaf");
						Err((0, None))
					}
				}
			}
		}
	}

	pub fn get<T>(&self, key: T) -> Option<&V> where K: BoundPartialOrd + RangeOrd<T> {
		println!("get");
		match self.address_of(&key, true) {
			Ok(addr) => {
				println!("found {}", addr);
				Some(self.btree.item(addr).unwrap().value())
			},
			Err(_) => {
				println!("not found");
				None
			}
		}
	}

	pub fn insert(&mut self, key: K, value: V) where K: BoundPartialOrd, V: PartialEq + Clone {
		self.insert_range(key.into(), value)
	}

	/// Insert a new key-value binding.
	pub fn insert_range(&mut self, key: Range<K>, mut value: V) where K: BoundPartialOrd, V: PartialEq + Clone {
		println!("insert_range");
		match self.address_of(&key, false) {
			Ok(mut addr) => {
				println!("found connected range");
				// some work to do here...
				loop {
					if self.btree.item(addr).map(|item| item.key().connected_to(&key)).unwrap_or(false) {
						match self.btree.item(addr).unwrap().key().without(&key) {
							(Some(left), Some(right)) => { // case (A)
								println!("(A)");
								let left = left.cloned();
								let right = right.cloned();

								if self.btree.item(addr).unwrap().value() == &value {
									self.btree.item_mut(addr).unwrap().key_mut().add(&key)
								} else {
									let right_value = {
										let item = self.btree.item_mut(addr).unwrap();
										*item.key_mut() = right;
										item.value().clone()
									};
									
									addr = self.btree.insert_at(addr, Item::new(key.into(), value));
	
									self.btree.insert_at(addr, Item::new(left, right_value));
								}

								return // no need to go further, the inserted range was totaly included in this one.
							},
							(Some(left), None) => { // case (B)
								println!("(B)");
								let left = left.cloned();

								if self.btree.item(addr).unwrap().value() == &value {
									self.btree.item_mut(addr).unwrap().key_mut().add(&key)
								} else {
									let left_value = {
										let item = self.btree.item_mut(addr).unwrap();
										*item.key_mut() = key.into();
										std::mem::swap(&mut value, item.value_mut());
										value
									};
	
									self.btree.insert_at(addr, Item::new(left, left_value));
								}
								
								return // no need to go further, the inserted range does not intersect anything below this range.
							},
							(None, Some(right)) => { // case (C)
								println!("(C)");
								let right = right.cloned();

								let item = self.btree.item_mut(addr).unwrap();
								*item.key_mut() = right;
							},
							(None, None) => { // case (D)
								println!("(D)");
								let (_, next_addr) = self.btree.remove_at(addr).unwrap();
								addr = next_addr
							}
						}

						match self.btree.previous_address(addr) {
							Some(prev_addr) => addr = prev_addr,
							None => { // case (E)
								println!("(E)");
								self.btree.insert_at(addr, Item::new(key.into(), value));
								return
							}
						}
					} else { // case (F)
						println!("(F)");
						self.btree.insert_at(addr, Item::new(key.into(), value));
						return
					}
				}
			},
			Err(addr) => { // case (G)
				println!("(G) free insert at {}", addr);
				// there are no connected ranges. We can freely insert this new range.
				self.btree.insert_at(addr, Item::new(key.into(), value));
			}
		}
	}

	/// Remove a key.
	///
	/// Returns the value that was bound to this key, if any.
	pub fn remove_range(&mut self, key: Range<K>) where K: BoundPartialOrd, V: Clone {
		match self.address_of(&key, true) {
			Ok(mut addr) => {
				loop {
					if self.btree.item(addr).map(|item| item.key().intersects(&key)).unwrap_or(false) {
						match self.btree.item(addr).unwrap().key().without(&key) {
							(Some(left), Some(right)) => {
								let left = left.cloned();
								let right = right.cloned();

								let right_value = {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = right;
									item.value().clone()
								};
								self.btree.insert_at(addr, Item::new(left, right_value));
								break // no need to go further, the removed range was totaly included in this one.
							},
							(Some(left), None) => {
								let left = left.cloned();
								let item = self.btree.item_mut(addr).unwrap();
								*item.key_mut() = left;
								break // no need to go further, the removed range does not intersect anything below this range.
							},
							(None, Some(right)) => {
								let right = right.cloned();
								let item = self.btree.item_mut(addr).unwrap();
								*item.key_mut() = right;
							},
							(None, None) => {
								let (_, next_addr) = self.btree.remove_at(addr).unwrap();
								addr = next_addr
							}
						}

						match self.btree.previous_address(addr) {
							Some(prev_addr) => addr = prev_addr,
							None => break
						}
					} else {
						break
					}
				}
			},
			Err(_) => ()
		}
	}
}

pub fn binary_search_with<T, U, V, I: AsRef<Item<Range<T>, V>>>(items: &[I], element: &U, disconnected: bool) -> Option<usize> where T: RangeOrd<U> {
	if items.is_empty() || T::range_cmp(items[0].as_ref().key(), element).is_after(disconnected) {
		None
	} else {
		let mut i = 0;
		let mut j = items.len() - 1;

		if !T::range_cmp(items[j].as_ref().key(), element).is_after(disconnected) { 
			return Some(j)
		}

		// invariants:
		// vec[i].as_ref().key() < range
		// vec[j].as_ref().key() >= range
		// j > i

		while j-i > 1 {
			let k = (i + j) / 2;

			if T::range_cmp(items[k].as_ref().key(), element).is_after(disconnected) {
				j = k;
			} else {
				i = k;
			}
		}

		Some(i)
	}
}