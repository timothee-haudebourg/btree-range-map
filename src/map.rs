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

pub struct RangeMap<K, V> {
	btree: BTreeMap<ExclusiveRange<K>, V>
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

	fn address_of<T: Clone, O: RangeOrd<K, T>>(&self, key: T) -> Result<ItemAddr, ItemAddr> {
		match self.btree.root_id() {
			Some(id) => self.address_in::<T, O>(id, key),
			None => Err(ItemAddr::nowhere())
		}
	}

	fn address_in<T: Clone, O: RangeOrd<K, T>>(&self, mut id: usize, key: T) -> Result<ItemAddr, ItemAddr> {
		loop {
			match self.offset_in::<T, O>(id, key.clone()) {
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

	fn offset_in<T: Clone, O: RangeOrd<K, T>>(&self, id: usize, key: T) -> Result<usize, (usize, Option<usize>)> {
		match self.btree.node(id) {
			Node::Internal(node) => {
				let branches = node.branches();
				match binary_search_with::<_, _, _, _, O>(branches, key.clone()) {
					Some(i) => {
						let b = &branches[i];
						if O::matches(b.item.key(), key) {
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
				match binary_search_with::<_, _, _, _, O>(items, key.clone()) {
					Some(i) => {
						let item = &items[i];
						println!("leaf found");
						if O::matches(item.key(), key) {
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

	pub fn get(&self, key: K) -> Option<&V> {
		println!("get");
		match self.address_of::<_, KeyOrd>(key) {
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

	pub fn insert(&mut self, key: K, value: V) where K: , V: PartialEq + Clone {
		self.insert_range(key.into(), value)
	}

	/// Insert a new key-value binding.
	pub fn insert_range(&mut self, key: Range<K>, mut value: V) where V: PartialEq + Clone {
		println!("insert_range");
		match self.address_of::<_, ConnectedOrd>(key.clone()) {
			Ok(mut addr) => {
				println!("found connected range");
				// some work to do here...
				loop {
					if self.btree.item(addr).map(|item| item.key().connected_to(key.clone())).unwrap_or(false) {
						match self.btree.item(addr).unwrap().key().without(key.clone()) {
							(Some(left), Some(right)) => { // case (A)
								println!("(A)");
								if self.btree.item(addr).unwrap().value() == &value {
									self.btree.item_mut(addr).unwrap().key_mut().add(key)
								} else {
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
								}

								return // no need to go further, the inserted range was totaly included in this one.
							},
							(Some(left), None) => { // case (B)
								println!("(B)");
								if self.btree.item(addr).unwrap().value() == &value {
									self.btree.item_mut(addr).unwrap().key_mut().add(key)
								} else {
									let left_value = {
										let item = self.btree.item_mut(addr).unwrap();
										*item.key_mut() = key.into();
										std::mem::swap(&mut value, item.value_mut());
										value
									};
	
									if !left.is_empty() {
										self.btree.insert_at(addr, Item::new(left, left_value));
									}
								}
								
								return // no need to go further, the inserted range does not intersect anything below this range.
							},
							(None, Some(right)) => { // case (C)
								println!("(C)");
								if !right.is_empty() {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = right;
								} else {
									let (_, next_addr) = self.btree.remove_at(addr).unwrap();
									addr = next_addr
								}
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
	pub fn remove_range(&mut self, key: Range<K>) where K: Clone, V: Clone {
		match self.address_of::<_, IntersectionOrd>(key.clone()) {
			Ok(mut addr) => {
				loop {
					if self.btree.item(addr).map(|item| item.key().intersects(key.clone())).unwrap_or(false) {
						match self.btree.item(addr).unwrap().key().without(key.clone()) {
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

pub struct KeyOrd;

impl<T: Clone + Ord + Debug> RangeOrd<T, T> for KeyOrd {
	fn cmp(a: &ExclusiveRange<T>, b: T) -> Ordering {
		println!("compare {:?} and {:?}", a, b);
		a.start.cmp(&b)
	}

	fn matches(a: &ExclusiveRange<T>, b: T) -> bool {
		a.contains(b)
	}
}

pub struct ConnectedOrd;

impl<T: Clone + Ord> RangeOrd<T, Range<T>> for ConnectedOrd {
	fn cmp(a: &ExclusiveRange<T>, b: Range<T>) -> Ordering {
		match a.start.cmp(&b.end) {
			Ordering::Equal => Ordering::Less,
			ordering => ordering
		}
	}

	fn matches(a: &ExclusiveRange<T>, b: Range<T>) -> bool {
		a.connected_to(b)
	}
}

pub struct IntersectionOrd;

impl<T: Clone + Ord + Debug> RangeOrd<T, Range<T>> for IntersectionOrd {
	fn cmp(a: &ExclusiveRange<T>, b: Range<T>) -> Ordering {
		println!("compare {:?} and {:?} = {:?}", a, b, a.start.cmp(&b.end));
		a.start.cmp(&b.end)
	}

	fn matches(a: &ExclusiveRange<T>, b: Range<T>) -> bool {
		a.intersects(b)
	}
}

pub fn binary_search_with<T: Clone + Ord, U: Clone, V, I: AsRef<Item<ExclusiveRange<T>, V>>, O: RangeOrd<T, U>>(items: &[I], element: U) -> Option<usize> {
	if items.is_empty() || O::cmp(items[0].as_ref().key(), element.clone()) == Ordering::Greater {
		None
	} else {
		let mut i = 0;
		let mut j = items.len() - 1;

		if O::cmp(items[j].as_ref().key(), element.clone()) != Ordering::Greater {
			return Some(j)
		}

		// invariants:
		// vec[i].as_ref().key() < range
		// vec[j].as_ref().key() >= range
		// j > i

		while j-i > 1 {
			let k = (i + j) / 2;

			if O::cmp(items[k].as_ref().key(), element.clone()) == Ordering::Greater {
				j = k;
			} else {
				i = k;
			}
		}

		Some(i)
	}
}