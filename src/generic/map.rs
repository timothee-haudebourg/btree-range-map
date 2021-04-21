use cc_traits::{
	Slab,
	SlabMut
};
use btree_slab::generic::{
	map::{
		BTreeMap,
		BTreeExt,
		BTreeExtMut
	},
	node::{
		Address,
		Offset,
		Node,
		Item
	}
};
use crate::{
	util::{
		Measure,
		Saturating
	},
	AnyRange,
	AsRange,
	RangeOrdering,
	RangePartialOrd
};

#[derive(Clone)]
pub struct RangeMap<K, V, C> {
	btree: BTreeMap<AnyRange<K>, V, C>
}

impl<K, V, C> RangeMap<K, V, C> {
	/// Create a new empty map.
	pub fn new() -> RangeMap<K, V, C> where C: Default {
		RangeMap {
			btree: BTreeMap::new()
		}
	}
}

impl<K, V, C: Slab<Node<AnyRange<K>, V>>> RangeMap<K, V, C> {
	pub fn len(&self) -> Saturating<K::Len> where K: Measure {
		let mut len = Saturating::<K::Len>::default();
		for (range, _) in self {
			len = len + range.len()
		}

		len
	}

	pub fn range_count(&self) -> usize {
		self.btree.len()
	}

	fn address_of<T>(&self, key: &T, connected: bool) -> Result<Address, Address> where K: Clone + PartialOrd + Measure, T: RangePartialOrd<K> {
		match self.btree.root_id() {
			Some(id) => self.address_in(id, key, connected),
			None => Err(Address::nowhere())
		}
	}

	fn address_in<T>(&self, mut id: usize, key: &T, connected: bool) -> Result<Address, Address> where K: Clone + PartialOrd + Measure, T: RangePartialOrd<K> {
		loop {
			match self.offset_in(id, key, connected) {
				Ok(offset) => {
					return Ok(Address::new(id, offset))
				},
				Err((offset, None)) => {
					return Err(Address::new(id, offset.into()))
				},
				Err((_, Some(child_id))) => {
					id = child_id;
				}
			}
		}
	}

	fn offset_in<T>(&self, id: usize, key: &T, connected: bool) -> Result<Offset, (usize, Option<usize>)> where K: Clone + PartialOrd + Measure, T: RangePartialOrd<K> {
		match self.btree.node(id) {
			Node::Internal(node) => {
				let branches = node.branches();
				match binary_search(branches, key, connected) {
					Some(i) => {
						let b = &branches[i];
						if key.range_partial_cmp(b.item.key()).unwrap_or(RangeOrdering::After(false)).matches(connected) {
							Ok(i.into())
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
				match binary_search(items, key, connected) {
					Some(i) => {
						let item = &items[i];
						let ord = key.range_partial_cmp(item.key()).unwrap_or(RangeOrdering::After(false));
						if ord.matches(connected) {
							Ok(i.into())
						} else {
							Err((i+1, None))
						}
					},
					None => {
						Err((0, None))
					}
				}
			}
		}
	}

	pub fn get(&self, key: K) -> Option<&V> where K: Clone + PartialOrd + RangePartialOrd + Measure {
		match self.address_of(&key, false) {
			Ok(addr) => {
				Some(self.btree.item(addr).unwrap().value())
			},
			Err(_) => {
				None
			}
		}
	}

	pub fn iter(&self) -> Iter<K, V, C> {
		self.btree.iter()
	}
}

impl<'a, K, V, C: Slab<Node<AnyRange<K>, V>>> IntoIterator for &'a RangeMap<K, V, C> {
	type Item = (&'a AnyRange<K>, &'a V);
	type IntoIter = Iter<'a, K, V, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<K, V, C: SlabMut<Node<AnyRange<K>, V>>> RangeMap<K, V, C> {
	pub fn update<R: AsRange<Item=K>, F>(&mut self, key: R, f: F) where K: Clone + PartialOrd + Measure, F: Fn(Option<&V>) -> Option<V>, V: PartialEq + Clone {
		let key = AnyRange::from(key);
		match self.address_of(&key, true) {
			Ok(mut addr) => {
				let mut next_addr = None;

				// some work to do here...
				loop {
					let item = self.btree.item(addr).unwrap();
					match item.key().without(&key) {
						(Some(left), Some(right)) => {
							let left = left.cloned();
							let right = right.cloned();

							match f(Some(item.value())) {
								Some(new_value) => { // new value to insert.
									if item.value() == &new_value {
										// nothing to do.
									} else {
										let right_value = {
											let item = self.btree.item_mut(addr).unwrap();
											*item.key_mut() = right;
											item.value().clone()
										};
										
										addr = self.btree.insert_at(addr, Item::new(key.into(), new_value));
										self.btree.insert_at(addr, Item::new(left, right_value));
									}
								},
								None => { // remove the item (split this range in two)
									let right_value = {
										let item = self.btree.item_mut(addr).unwrap();
										*item.key_mut() = right;
										item.value().clone()
									};
									self.btree.insert_at(addr, Item::new(left, right_value));
								}
							}

							// Because we have `Some(left)`
							// we know nothing on the left intersects the input range.
							break // we are done
						},
						(Some(left), None) => { // case (B)
							let left = left.cloned();

							match f(Some(item.value())) {
								Some(mut new_value) => { // new value to insert.
									let same_as_prev = item.value() == &new_value;
									let same_as_next = next_addr.map(|next_addr| self.btree.item(next_addr).unwrap().value() == &new_value).unwrap_or(false);

									if same_as_prev {
										if same_as_next {
											let next_item = self.btree.item_mut(next_addr.unwrap()).unwrap();
											next_item.key_mut().add(&left); // also absorb left
											self.btree.remove_at(addr);
										} else {
											// nothing to do.
										}
									} else {
										let right = key.intersected_with(item.key()).cloned();
										if same_as_next {
											let item = self.btree.item_mut(addr).unwrap();
											*item.key_mut() = left;

											let next_item = self.btree.item_mut(next_addr.unwrap()).unwrap();
											next_item.key_mut().add(&right);
										} else {
											let item = self.btree.item_mut(addr).unwrap();
											std::mem::swap(item.value_mut(), &mut new_value);
											*item.key_mut() = right;
											self.btree.insert_at(addr, Item::new(left, new_value));
										}
									}
								},
								None => {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = left;
								}
							}

							// Because we have `Some(left)`
							// we know nothing on the left intersects the input range.
							break // we are done
						},
						(None, Some(right)) => {
							let right = right.cloned();

							match f(Some(item.value())) {
								Some(new_value) => {
									if item.value() == &new_value {
										// nothing to do.
									} else {
										let item = self.btree.item_mut(addr).unwrap();
										*item.key_mut() = right;
										let left = key.intersected_with(item.key()).cloned();
										addr = self.btree.insert_at(addr, Item::new(left, new_value));
									}
								},
								None => {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = right;
								}
							}
						},
						(None, None) => {
							match f(Some(item.value())) {
								Some(new_value) => {
									let same_as_next = next_addr.map(|next_addr| self.btree.item(next_addr).unwrap().value() == &new_value).unwrap_or(false);
								
									if same_as_next {
										let item_key = item.key().clone();
										let next_item = self.btree.item_mut(next_addr.unwrap()).unwrap();
										next_item.key_mut().add(&item_key);
									} else {
										let item = self.btree.item_mut(addr).unwrap();
										item.set_value(new_value);
									}
								},
								None => {
									addr = self.btree.remove_at(addr).unwrap().1;
								}
							}
						}
					}

					// go to the previous item is it also intersects the input range.
					match self.btree.previous_item_address(addr) {
						Some(prev_addr) if self.btree.item(prev_addr).unwrap().key().connected_to(&key) => {
							next_addr = Some(addr);
							addr = prev_addr
						},
						_ => break // otherwise we're done.
					}
				}
			},
			Err(addr) => { // case (G)
				match f(None) {
					Some(new_value) => {
						self.btree.insert_at(addr, Item::new(key.into(), new_value));
					},
					None => () // nothing to do.
				}
			}
		}
	}

	/// Insert a new key-value binding.
	pub fn insert<R: AsRange<Item=K>>(&mut self, key: R, mut value: V) where K: Clone + PartialOrd + Measure, V: PartialEq + Clone {
		let key = AnyRange::from(key);
		match self.address_of(&key, true) {
			Ok(mut addr) => {
				let mut next_addr = None;

				// some work to do here...
				loop {
					let item = self.btree.item(addr).unwrap();
					match item.key().without(&key) {
						(Some(left), Some(right)) => {
							let left = left.cloned();
							let right = right.cloned();

							if item.value() == &value {
								// nothing to do.
							} else {
								let right_value = {
									let item = self.btree.item_mut(addr).unwrap();
									*item.key_mut() = right;
									item.value().clone()
								};
								
								addr = self.btree.insert_at(addr, Item::new(key.into(), value));
								self.btree.insert_at(addr, Item::new(left, right_value));
							}

							// Because we have `Some(left)`
							// we know nothing on the left intersects the input range.
							break // we are done
						},
						(Some(left), None) => {
							let left = left.cloned();

							let same_as_prev = item.value() == &value;
							let same_as_next = next_addr.is_some();

							if same_as_prev {
								if same_as_next {
									let next_item = self.btree.item_mut(next_addr.unwrap()).unwrap();
									next_item.key_mut().add(&left);
									self.btree.remove_at(addr);
								} else {
									let item = self.btree.item_mut(addr).unwrap();
									item.key_mut().add(&key);
								}
							} else {
								if same_as_next {
									// nothing to do.
								} else {
									let item = self.btree.item_mut(addr).unwrap();
									std::mem::swap(item.value_mut(), &mut value);
									*item.key_mut() = key.clone().into();
									self.btree.insert_at(addr, Item::new(left, value));
								}
							}

							// Because we have `Some(left)`
							// we know nothing on the left intersects the input range.
							break // we are done
						},
						(None, Some(right)) => {
							let right = right.cloned();

							if item.value() == &value {
								let item = self.btree.item_mut(addr).unwrap();
								item.key_mut().add(&key);
							} else {
								let item = self.btree.item_mut(addr).unwrap();
								*item.key_mut() = right;
								addr = self.btree.insert_at(addr, Item::new(key.clone().into(), value.clone()));
							}
						},
						(None, None) => {
							let same_as_next = next_addr.map(|next_addr| self.btree.item(next_addr).unwrap().value() == &value).unwrap_or(false);
								
							if same_as_next {
								self.btree.remove_at(addr);
							} else {
								let item = self.btree.item_mut(addr).unwrap();
								item.key_mut().add(&key);
								item.set_value(value.clone());
							}
						}
					}

					// go to the previous item is it also intersects the input range.
					match self.btree.previous_item_address(addr) {
						Some(prev_addr) if self.btree.item(prev_addr).unwrap().key().connected_to(&key) => {
							next_addr = Some(addr);
							addr = prev_addr
						},
						_ => break // otherwise we're done.
					}
				}
			},
			Err(addr) => { // case (G)
				self.btree.insert_at(addr, Item::new(key.into(), value));
			}
		}
	}

	/// Remove a key.
	pub fn remove<R: AsRange<Item=K>>(&mut self, key: R) where K: Clone + PartialOrd + Measure, V: Clone {
		let key = AnyRange::from(key);
		match self.address_of(&key, false) {
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

						match self.btree.previous_item_address(addr) {
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

	pub fn into_iter(self) -> IntoIter<K, V, C> {
		self.btree.into_iter()
	}
}

impl<K, V, C: SlabMut<Node<AnyRange<K>, V>>> IntoIterator for RangeMap<K, V, C> {
	type Item = (AnyRange<K>, V);
	type IntoIter = IntoIter<K, V, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.into_iter()
	}
}

/// Search for the index of the gratest item less/below or equal/including the given element.
/// 
/// If `connected` is `true`, then it will search for the gratest item less/below or equal/including **or connected to** the given element.
pub fn binary_search<T: Measure + PartialOrd, U, V, I: AsRef<Item<AnyRange<T>, V>>>(items: &[I], element: &U, connected: bool) -> Option<usize> where U: RangePartialOrd<T> {
	if items.is_empty() || element.range_partial_cmp(items[0].as_ref().key()).unwrap_or(RangeOrdering::Before(false)).is_before(connected) {
		None
	} else {
		let mut i = 0;
		let mut j = items.len() - 1;

		if !element.range_partial_cmp(items[j].as_ref().key()).unwrap_or(RangeOrdering::After(false)).is_before(connected) { 
			return Some(j)
		}

		// invariants:
		// vec[i].as_ref().key() < range
		// vec[j].as_ref().key() >= range
		// j > i

		while j-i > 1 {
			let k = (i + j) / 2;

			if let Some(ord) = element.range_partial_cmp(items[k].as_ref().key()) {
				eprintln!("ord: {:?}", ord);
				if ord.is_before(connected) {
					j = k;
				} else {
					i = k;
				}
			} else {
				return None // FIXME: that's bad. Maybe we should expect a total order.
			}
		}

		Some(i)
	}
}

pub type Iter<'a, K, V, C> = btree_slab::generic::map::Iter<'a, AnyRange<K>, V, C>;
pub type IntoIter<K, V, C> = btree_slab::generic::map::IntoIter<AnyRange<K>, V, C>;

#[cfg(test)]
mod tests {
	use super::*;

	macro_rules! items {
		[$($item:expr),*] => {
			&[
				$(
					Item::new(AnyRange::from($item), ())
				),*
			]
		};
	}

	#[test]
	fn binary_search_disconnected_singletons() {
		assert_eq!(binary_search(items![0], &0, false), Some(0));

		assert_eq!(binary_search(items![0, 2, 4], &0, false), Some(0));
		assert_eq!(binary_search(items![0, 2, 4], &1, false), Some(0));
		assert_eq!(binary_search(items![0, 2, 4], &2, false), Some(1));
		assert_eq!(binary_search(items![0, 2, 4], &3, false), Some(1));
		assert_eq!(binary_search(items![0, 2, 4], &4, false), Some(2));
		assert_eq!(binary_search(items![0, 2, 4], &5, false), Some(2));

		assert_eq!(binary_search(items![0, 3, 6], &0, false), Some(0));
		assert_eq!(binary_search(items![0, 3, 6], &1, false), Some(0));
		assert_eq!(binary_search(items![0, 3, 6], &2, false), Some(0));
		assert_eq!(binary_search(items![0, 3, 6], &3, false), Some(1));
		assert_eq!(binary_search(items![0, 3, 6], &4, false), Some(1));
		assert_eq!(binary_search(items![0, 3, 6], &5, false), Some(1));
		assert_eq!(binary_search(items![0, 3, 6], &6, false), Some(2));
		assert_eq!(binary_search(items![0, 3, 6], &7, false), Some(2));
	}

	#[test]
	fn binary_search_disconnected_singletons_float() {
		assert_eq!(binary_search(items![0.0], &0.0, false), Some(0));

		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &-1.0, false), None);
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &0.0, false), Some(0));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &1.0, false), Some(0));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &2.0, false), Some(1));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &3.0, false), Some(1));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &4.0, false), Some(2));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &5.0, false), Some(2));

		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &0.0, false), Some(0));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &1.0, false), Some(0));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &2.0, false), Some(0));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &3.0, false), Some(1));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &4.0, false), Some(1));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &5.0, false), Some(1));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &6.0, false), Some(2));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &7.0, false), Some(2));
	}

	#[test]
	fn binary_search_connected_singletons() {
		assert_eq!(binary_search(items![0], &0, true), Some(0));

		assert_eq!(binary_search(items![0, 2, 4], &0, true), Some(0));
		assert_eq!(binary_search(items![0, 2, 4], &1, true), Some(1));
		assert_eq!(binary_search(items![0, 2, 4], &2, true), Some(1));
		assert_eq!(binary_search(items![0, 2, 4], &3, true), Some(2));
		assert_eq!(binary_search(items![0, 2, 4], &4, true), Some(2));
		assert_eq!(binary_search(items![0, 2, 4], &5, true), Some(2));
		assert_eq!(binary_search(items![2, 4, 8], &0, true), None);

		assert_eq!(binary_search(items![0, 3, 6], &0, true), Some(0));
		assert_eq!(binary_search(items![0, 3, 6], &1, true), Some(0));
		assert_eq!(binary_search(items![0, 3, 6], &2, true), Some(1));
		assert_eq!(binary_search(items![0, 3, 6], &3, true), Some(1));
		assert_eq!(binary_search(items![0, 3, 6], &4, true), Some(1));
		assert_eq!(binary_search(items![0, 3, 6], &5, true), Some(2));
		assert_eq!(binary_search(items![0, 3, 6], &6, true), Some(2));
		assert_eq!(binary_search(items![0, 3, 6], &7, true), Some(2));
	}

	// for floats, connected or disconnected makes no difference for singletons.
	#[test]
	fn binary_search_connected_singletons_float() {
		assert_eq!(binary_search(items![0.0], &0.0, true), Some(0));

		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &-1.0, true), None);
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &0.0, true), Some(0));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &1.0, true), Some(0));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &2.0, true), Some(1));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &3.0, true), Some(1));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &4.0, true), Some(2));
		assert_eq!(binary_search(items![0.0, 2.0, 4.0], &5.0, true), Some(2));

		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &0.0, true), Some(0));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &1.0, true), Some(0));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &2.0, true), Some(0));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &3.0, true), Some(1));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &4.0, true), Some(1));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &5.0, true), Some(1));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &6.0, true), Some(2));
		assert_eq!(binary_search(items![0.0, 3.0, 6.0], &7.0, true), Some(2));
	}
}