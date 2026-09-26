use super::Node;
use crate::{
	AnyRange, AsRange, IntoRange, RangeOrdering, RangePartialOrd,
	range::{Difference, ProductArg},
};
use range_traits::{Bounded, Measure, PartialEnum};
use raw_btree::{Address, Item, RawBTree, Storage, node::Offset};
use std::{
	cmp::{Ord, Ordering, PartialOrd},
	fmt,
	hash::{Hash, Hasher},
};

/// Range map.
pub struct RangeMap<K, V, C: Storage<Item<AnyRange<K>, V>>> {
	btree: RawBTree<Item<AnyRange<K>, V>, C>,
}

impl<K: Clone, V: Clone, C: Storage<Item<AnyRange<K>, V>>> Clone for RangeMap<K, V, C> {
	fn clone(&self) -> Self {
		RangeMap {
			btree: self.btree.clone(),
		}
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> RangeMap<K, V, C> {
	/// Create a new empty map.
	pub fn new() -> RangeMap<K, V, C> {
		RangeMap {
			btree: RawBTree::new(),
		}
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> Default for RangeMap<K, V, C> {
	fn default() -> Self {
		Self::new()
	}
}

pub struct CandidateOffset<N> {
	pub offset: Result<Offset, Offset>,
	pub node: Option<N>,
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> RangeMap<K, V, C> {
	pub fn len(&self) -> K::Len
	where
		K: Measure + PartialEnum + Bounded,
	{
		let mut len = K::Len::default();
		for (range, _) in self {
			len = len + range.len()
		}

		len
	}

	pub fn bounded_len(&self) -> Option<K::Len>
	where
		K: Measure + PartialEnum,
	{
		let mut len = K::Len::default();
		for (range, _) in self {
			len = len + range.bounded_len()?
		}

		Some(len)
	}

	pub fn is_empty(&self) -> bool
	where
		K: Measure + PartialEnum,
	{
		self.bounded_len() == Some(K::Len::default())
	}

	pub fn range_count(&self) -> usize {
		self.btree.len()
	}

	fn address_of<T>(
		&self,
		key: &T,
		connected: bool,
	) -> Result<Address<C::Node>, Option<Address<C::Node>>>
	where
		K: PartialEnum + Measure,
		T: RangePartialOrd<K>,
	{
		if connected && let Ok(addr) = self.address_of(key, false) {
			return Ok(addr);
		}

		match self.btree.root() {
			Some(id) => self.address_in(id, key, connected).map_err(Some),
			None => Err(None),
		}
	}

	fn address_in<T>(
		&self,
		mut id: C::Node,
		key: &T,
		connected: bool,
	) -> Result<Address<C::Node>, Address<C::Node>>
	where
		K: PartialEnum + Measure,
		T: RangePartialOrd<K>,
	{
		let mut candidate = None;

		loop {
			match self.offset_in(id, key, connected) {
				CandidateOffset {
					offset: Ok(offset),
					node: None,
				} => {
					// Found the best match!
					return Ok(Address::new(id, offset));
				}
				CandidateOffset {
					offset: Ok(offset),
					node: Some(child_id),
				} => {
					// Found a candidate, but a better one may be deeper in the tree.
					candidate = Some(Address::new(id, offset));
					id = child_id;
				}
				CandidateOffset {
					offset: Err(_),
					node: Some(child_id),
				} => {
					// No candidate here, but one may be deeper in the tree.
					id = child_id;
				}
				CandidateOffset {
					offset: Err(offset),
					node: None,
				} => {
					// We won't find any more candidates.
					return candidate.ok_or(Address::new(id, offset));
				}
			}
		}
	}

	fn offset_in<T>(&self, id: C::Node, key: &T, connected: bool) -> CandidateOffset<C::Node>
	where
		K: PartialEnum + Measure,
		T: RangePartialOrd<K>,
	{
		match unsafe { self.btree.node(id) } {
			Node::Internal(node) => {
				let branches = node.branches();
				match binary_search(branches, key, connected) {
					Some(i) => {
						let b = &branches[i];
						if key
							.range_partial_cmp(&b.item.key)
							.unwrap_or(RangeOrdering::After(false))
							.matches(connected)
						{
							CandidateOffset {
								offset: Ok(i.into()),
								node: Some(b.child),
							}
						} else {
							CandidateOffset {
								offset: Err(i.into()),
								node: Some(b.child),
							}
						}
					}
					None => CandidateOffset {
						offset: Err(0.into()),
						node: Some(node.first_child_id()),
					},
				}
			}
			Node::Leaf(leaf) => {
				let items = leaf.items();
				match binary_search(items, key, connected) {
					Some(i) => {
						let item = &items[i];
						let ord = key
							.range_partial_cmp(&item.key)
							.unwrap_or(RangeOrdering::After(false));
						if ord.matches(connected) {
							CandidateOffset {
								offset: Ok(i.into()),
								node: None,
							}
						} else {
							CandidateOffset {
								offset: Err((i + 1).into()),
								node: None,
							}
						}
					}
					None => CandidateOffset {
						offset: Err(0.into()),
						node: None,
					},
				}
			}
		}
	}

	/// # Complexity
	///
	/// `O(log n)`, where `n` is the number of ranges in the map
	/// ([`Self::range_count`]). A single top-down search through the B-tree
	/// locates the (at most one) range intersecting `key`.
	pub fn intersects<R: AsRange<Item = K>>(&self, key: R) -> bool
	where
		K: PartialEnum + Measure,
		V: PartialEq,
	{
		// let key = AnyRange::from(key);

		if key.is_empty() {
			false
		} else {
			self.address_of(&key, false).is_ok()
		}
	}

	/// # Complexity
	///
	/// `O(log n)`, where `n` is the number of ranges in the map
	/// ([`Self::range_count`]). Same single-search cost as [`Self::get`].
	pub fn contains_key(&self, key: K) -> bool
	where
		K: PartialEnum + RangePartialOrd + Measure,
	{
		self.address_of(&key, false).is_ok()
	}

	/// # Complexity
	///
	/// `O(log n)`, where `n` is the number of ranges in the map
	/// ([`Self::range_count`]). A single top-down binary search through the
	/// B-tree locates the range containing `key`.
	pub fn get(&self, key: K) -> Option<&V>
	where
		K: PartialEnum + RangePartialOrd + Measure,
	{
		match self.address_of(&key, false) {
			Ok(addr) => Some(&unsafe { self.btree.get_at(addr) }.unwrap().value),
			Err(_) => None,
		}
	}

	pub fn iter(&self) -> Iter<'_, K, V, C> {
		Iter {
			inner: self.btree.iter(),
		}
	}

	/// Returns an iterator over the gaps (unbounded keys) of the map.
	pub fn gaps(&self) -> Gaps<'_, K, V, C> {
		Gaps {
			inner: self.iter(),
			prev: None,
			done: false,
		}
	}
}

impl<K: fmt::Debug, V: fmt::Debug, C: Storage<Item<AnyRange<K>, V>>> fmt::Debug
	for RangeMap<K, V, C>
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{")?;

		for (range, value) in self {
			write!(f, "{:?}=>{:?}", range, value)?
		}

		write!(f, "}}")
	}
}

impl<K, V, C, D> PartialEq<RangeMap<K, V, D>> for RangeMap<K, V, C>
where
	K: Measure + PartialOrd + PartialEnum,
	V: PartialEq,
	C: Storage<Item<AnyRange<K>, V>>,
	D: Storage<Item<AnyRange<K>, V>>,
{
	fn eq(&self, other: &RangeMap<K, V, D>) -> bool {
		self.iter().eq(other.iter())
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> Eq for RangeMap<K, V, C>
where
	K: Measure + PartialEnum + Ord,
	V: Eq,
{
}

impl<K, V, C, D> PartialOrd<RangeMap<K, V, D>> for RangeMap<K, V, C>
where
	K: Measure + PartialOrd + PartialEnum,
	V: PartialOrd,
	C: Storage<Item<AnyRange<K>, V>>,
	D: Storage<Item<AnyRange<K>, V>>,
{
	fn partial_cmp(&self, other: &RangeMap<K, V, D>) -> Option<Ordering> {
		self.iter().partial_cmp(other.iter())
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> Ord for RangeMap<K, V, C>
where
	K: Measure + PartialEnum + Ord,
	V: Ord,
{
	fn cmp(&self, other: &Self) -> Ordering {
		self.iter().cmp(other.iter())
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> Hash for RangeMap<K, V, C>
where
	K: Hash + PartialEnum,
	V: Hash,
{
	fn hash<H: Hasher>(&self, h: &mut H) {
		for range in self {
			range.hash(h)
		}
	}
}

impl<'a, K, V, C: Storage<Item<AnyRange<K>, V>>> IntoIterator for &'a RangeMap<K, V, C> {
	type Item = (&'a AnyRange<K>, &'a V);
	type IntoIter = Iter<'a, K, V, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> RangeMap<K, V, C> {
	fn merge_forward(&mut self, addr: Address<C::Node>, next_addr: Option<Address<C::Node>>)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		if let Some(next_addr) = next_addr {
			// SAFETY: `addr` is a valid address in this tree.
			let item = unsafe { self.btree.get_at(addr) }.unwrap();
			// SAFETY: `next_addr` is a valid address in this tree.
			let next_item = unsafe { self.btree.get_at(next_addr) }.unwrap();
			if item.key.connected_to(&next_item.key) && item.value == next_item.value {
				// SAFETY: `addr` is a valid address in this tree.
				let (removed_item, non_normalized_new_addr) =
					unsafe { self.btree.remove_at(addr) }.unwrap();
				let new_addr = non_normalized_new_addr
					.and_then(|a| {
						// SAFETY: `a` was just returned by `remove_at` as a valid address.
						unsafe { self.btree.normalize(a) }
					})
					.unwrap();
				// SAFETY: `new_addr` was just returned by `normalize` as a valid address.
				let item = unsafe { self.btree.get_mut_at(new_addr) }.unwrap();
				item.key.add(&removed_item.key);
			}
		}
	}

	fn set_item_key(
		&mut self,
		addr: Address<C::Node>,
		next_addr: Option<Address<C::Node>>,
		new_key: AnyRange<K>,
	) -> (Address<C::Node>, Option<Address<C::Node>>)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		if let Some(next_addr) = next_addr {
			// SAFETY: `next_addr` is a valid address in this tree.
			let next_item = unsafe { self.btree.get_at(next_addr) }.unwrap();
			// SAFETY: `addr` is a valid address in this tree.
			let addr_value = &unsafe { self.btree.get_at(addr) }.unwrap().value;
			if new_key.connected_to(&next_item.key) && next_item.value == *addr_value {
				// Merge with the next item.
				// SAFETY: `addr` is a valid address in this tree.
				let (_, non_normalized_new_addr) = unsafe { self.btree.remove_at(addr) }.unwrap();
				let new_addr = non_normalized_new_addr
					.and_then(|a| {
						// SAFETY: `a` was just returned by `remove_at` as a valid address.
						unsafe { self.btree.normalize(a) }
					})
					.unwrap();
				// SAFETY: `new_addr` was just returned by `normalize` as a valid address.
				let item = unsafe { self.btree.get_mut_at(new_addr) }.unwrap();
				item.key.add(&new_key);

				// SAFETY: `new_addr` was just returned by `normalize` as a valid address.
				let next_addr = unsafe { self.btree.next_item_address(new_addr) };
				return (new_addr, next_addr);
			}
		}

		// SAFETY: `addr` is a valid address in this tree.
		let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
		item.key = new_key;
		(addr, next_addr)
	}

	fn set_item(
		&mut self,
		addr: Address<C::Node>,
		next_addr: Option<Address<C::Node>>,
		new_key: AnyRange<K>,
		new_value: V,
	) -> SetItem<C::Node, V>
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		if let Some(next_addr) = next_addr {
			// SAFETY: `next_addr` is a valid address in this tree.
			let next_item = unsafe { self.btree.get_at(next_addr) }.unwrap();
			if new_key.connected_to(&next_item.key) && next_item.value == new_value {
				// Merge with the next item.
				// SAFETY: `addr` is a valid address in this tree.
				let (removed_item, non_normalized_new_addr) =
					unsafe { self.btree.remove_at(addr) }.unwrap();
				let new_addr = non_normalized_new_addr
					.and_then(|a| {
						// SAFETY: `a` was just returned by `remove_at` as a valid address.
						unsafe { self.btree.normalize(a) }
					})
					.unwrap();
				// SAFETY: `new_addr` was just returned by `normalize` as a valid address.
				let item = unsafe { self.btree.get_mut_at(new_addr) }.unwrap();
				item.key.add(&new_key);

				// SAFETY: `new_addr` was just returned by `normalize` as a valid address.
				let after_addr = unsafe { self.btree.next_item_address(new_addr) };
				return SetItem::new(new_addr, after_addr, removed_item.value);
			}
		}

		// SAFETY: `addr` is a valid address in this tree.
		let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
		let removed_value = std::mem::replace(&mut item.value, new_value);
		item.key = new_key;
		SetItem::new(addr, next_addr, removed_value)
	}

	fn insert_item(
		&mut self,
		addr: Address<C::Node>,
		key: AnyRange<K>,
		value: V,
	) -> (Address<C::Node>, Option<Address<C::Node>>)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		// SAFETY: `addr` is a valid address in this tree.
		let next_item = unsafe { self.btree.get_at(addr) }.unwrap();
		if key.connected_to(&next_item.key) && next_item.value == value {
			// Merge with the next item.
			// SAFETY: `addr` is a valid address in this tree.
			let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
			item.key.add(&key);

			// SAFETY: `addr` is a valid address in this tree.
			let next_addr = unsafe { self.btree.next_item_address(addr) };
			return (addr, next_addr);
		}

		// SAFETY: `addr` is a valid address in this tree (`Some` is always
		// returned by `insert_at` when inserting, since insertion can only
		// overflow a node, never leave the tree empty).
		let new_addr = unsafe { self.btree.insert_at(Some(addr), Item::new(key, value)) }.unwrap();
		// SAFETY: `new_addr` was just returned by `insert_at` as a valid address.
		let next_addr = unsafe { self.btree.next_item_address(new_addr) };
		(new_addr, next_addr)
	}

	fn remove_item(
		&mut self,
		addr: Address<C::Node>,
	) -> (Address<C::Node>, Option<Address<C::Node>>) {
		// SAFETY: `addr` is a valid address in this tree.
		let (_, non_normalized_addr) = unsafe { self.btree.remove_at(addr) }.unwrap();
		let non_normalized_addr = non_normalized_addr.expect("range map unexpectedly became empty");
		// SAFETY: `non_normalized_addr` was just returned by `remove_at` as a
		// valid address.
		let new_addr = unsafe { self.btree.previous_item_address(non_normalized_addr) }.unwrap();
		// SAFETY: `non_normalized_addr` was just returned by `remove_at` as a
		// valid address.
		let normalized_addr = unsafe { self.btree.normalize(non_normalized_addr) };
		(new_addr, normalized_addr)
	}

	/// # Complexity
	///
	/// `O((k + 1) log n)`, where `n` is the number of ranges in the map
	/// ([`Self::range_count`]) and `k` is the number of existing ranges that
	/// overlap, or are connected to, `key`. Each of the `k` affected ranges is
	/// merged, split or removed with one `O(log n)` B-tree operation, on top
	/// of the initial `O(log n)` lookup. This is `O(log n)` when `key` only
	/// touches a handful of existing ranges (the common case), and `O(n log
	/// n)` in the worst case (`key` overlaps or connects to every stored
	/// range). This primitive underlies
	/// [`RangeMultiMap::insert`](crate::generic::RangeMultiMap::insert) and
	/// [`RangeMultiMap::remove`](crate::generic::RangeMultiMap::remove).
	pub fn update<R: AsRange<Item = K>, F>(&mut self, key: R, f: F)
	where
		K: Clone + PartialEnum + Measure,
		F: Fn(Option<&V>) -> Option<V>,
		V: PartialEq + Clone,
	{
		let mut key = AnyRange::from(key);

		if key.is_empty() {
			return;
		}

		match self.address_of(&key, true) {
			Ok(mut addr) => {
				// SAFETY: `addr` is a valid address in this tree.
				let mut next_addr = unsafe { self.btree.next_item_address(addr) };

				loop {
					let (prev_addr, prev_next_addr) = {
						// SAFETY: `addr` is a valid address in this tree.
						let addr_key = &unsafe { self.btree.get_at(addr) }.unwrap().key;
						let product = key.product(addr_key).cloned();

						let mut removed_item_value = None;

						let (addr, next_addr) = match product.after {
							Some(ProductArg::Subject(key_after)) => {
								match f(None) {
									Some(value) => {
										let SetItem {
											new_addr,
											new_next_addr,
											removed_value,
										} = self.set_item(addr, next_addr, key_after, value);
										removed_item_value = Some(removed_value);
										(new_addr, new_next_addr)
									}
									None => (addr, next_addr), // we wait the last minute to remove the item.
								}
							}
							Some(ProductArg::Object(item_after)) => {
								// SAFETY: `addr` is a valid address in this tree.
								let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
								item.key = item_after;
								removed_item_value = Some(item.value.clone());
								(addr, next_addr)
							}
							None => (addr, next_addr), // we wait the last minute to remove the item.
						};

						let (addr, next_addr) = match product.intersection {
							Some(intersection) => {
								let new_value = match removed_item_value.as_ref() {
									Some(value) => f(Some(value)),
									None => {
										// SAFETY: `addr` is a valid address in this tree.
										let value =
											&unsafe { self.btree.get_at(addr) }.unwrap().value;
										f(Some(value))
									}
								};

								match new_value {
									Some(new_value) => {
										if removed_item_value.is_some() {
											let (new_addr, new_next_addr) =
												self.insert_item(addr, intersection, new_value);
											(new_addr, new_next_addr)
										} else {
											let SetItem {
												new_addr,
												new_next_addr,
												removed_value,
											} = self.set_item(
												addr,
												next_addr,
												intersection,
												new_value,
											);
											removed_item_value = Some(removed_value);
											(new_addr, new_next_addr)
										}
									}
									None => (addr, next_addr), // we wait the last minute to remove the item.
								}
							}
							None => (addr, next_addr), // we wait the last minute to remove the item.
						};

						match product.before {
							Some(ProductArg::Subject(key_before)) => {
								// SAFETY: `addr` is a valid address in this tree. The
								// closure is only ever called with `prev_addr` values
								// returned by `previous_item_address` itself.
								let prev = unsafe { self.btree.previous_item_address(addr) }
									.filter(|&prev_addr| {
										// SAFETY: `prev_addr` was just returned by
										// `previous_item_address` as a valid address.
										unsafe { self.btree.get_at(prev_addr) }
											.unwrap()
											.key
											.connected_to(&key_before)
									});

								match prev {
									Some(prev_addr) => {
										let (prev_addr, addr) = if removed_item_value.is_none() {
											self.remove_item(addr)
										} else {
											(prev_addr, Some(addr))
										};

										// Let's go for another turn!
										// One item back this time.
										key = key_before;
										(prev_addr, addr)
									}
									None => {
										// there is no previous connected item, we must insert here!
										match f(None) {
											Some(value) => {
												if removed_item_value.is_some() {
													// we cannot reuse the item
													// insert
													self.insert_item(addr, key_before, value);
												} else {
													// we can reuse the item
													// reuse
													self.set_item(
														addr, next_addr, key_before, value,
													);
												}
											}
											None => {
												if removed_item_value.is_none() {
													// finally remove the item.
													// SAFETY: `addr` is a valid address in this tree.
													unsafe { self.btree.remove_at(addr) };
												}
											}
										}

										break;
									}
								}
							}
							Some(ProductArg::Object(item_before)) => {
								match removed_item_value {
									Some(value) => {
										self.insert_item(addr, item_before, value);
									}
									None => {
										self.set_item_key(addr, next_addr, item_before);
									}
								}

								break;
							}
							None => {
								// SAFETY: `addr` is a valid address in this tree.
								match unsafe { self.btree.previous_item_address(addr) } {
									Some(prev_addr) => {
										let (prev_addr, addr) = if removed_item_value.is_none() {
											self.remove_item(addr)
										} else {
											(prev_addr, Some(addr))
										};

										self.merge_forward(prev_addr, addr)
									}
									_ => {
										if removed_item_value.is_none() {
											// SAFETY: `addr` is a valid address in this tree.
											unsafe { self.btree.remove_at(addr) }.unwrap();
										}
									}
								}

								break;
							}
						}
					};

					addr = prev_addr;
					next_addr = prev_next_addr;
				}
			}
			Err(addr) => {
				// case (G)
				if let Some(new_value) = f(None) {
					// SAFETY: `addr` is `None` only if the tree is empty, which
					// `insert_at` handles.
					unsafe { self.btree.insert_at(addr, Item::new(key, new_value)) };
				}
			}
		}

		for (range, _) in self.iter() {
			debug_assert!(!range.is_empty());
		}
	}

	pub fn insert_disconnected<R: IntoRange<Item = K>>(
		&mut self,
		key: R,
		value: V,
	) -> Result<(), (AnyRange<K>, V)>
	where
		K: PartialEnum + Measure,
	{
		let key = key.into_range();
		match self.address_of(&key, true) {
			Ok(_) => Err((key, value)),
			Err(addr) => {
				unsafe {
					self.btree.insert_at(addr, Item::new(key, value));
				}
				Ok(())
			}
		}
	}

	/// Insert a new key-value binding.
	///
	/// # Complexity
	///
	/// `O((k + 1) log n)`, where `n` is the number of ranges in the map
	/// ([`Self::range_count`]) and `k` is the number of existing ranges that
	/// overlap, or are connected to, `key`. Each of the `k` affected ranges is
	/// merged, split or replaced with one `O(log n)` B-tree operation, on top
	/// of the initial `O(log n)` lookup. Usually `O(log n)`; worst case (e.g.
	/// inserting a range spanning the whole map) is `O(n log n)`.
	pub fn insert<R: IntoRange<Item = K>>(&mut self, key: R, value: V)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq + Clone,
	{
		let mut key = key.into_range();

		if key.is_empty() {
			return;
		}

		match self.address_of(&key, true) {
			Ok(mut addr) => {
				// let mut value = Some(value);
				// SAFETY: `addr` is a valid address in this tree.
				let mut next_addr = unsafe { self.btree.next_item_address(addr) };

				loop {
					let (prev_addr, prev_next_addr) = {
						// SAFETY: `addr` is a valid address in this tree.
						let addr_key = &unsafe { self.btree.get_at(addr) }.unwrap().key;
						let product = key.product(addr_key).cloned();

						let mut removed_item_value = None;

						if let Some(ProductArg::Object(item_after)) = product.after {
							// SAFETY: `addr` is a valid address in this tree.
							let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
							item.key = item_after;
							removed_item_value = Some(item.value.clone());
						}

						match product.before {
							Some(ProductArg::Object(item_before)) => {
								match removed_item_value {
									Some(old_value) => {
										if old_value == value {
											key.add(&item_before);
											self.insert_item(addr, key, value);
										} else {
											let (addr, _) = self.insert_item(addr, key, value);
											self.insert_item(addr, item_before, old_value);
										}
									}
									None => {
										// SAFETY: `addr` is a valid address in this tree.
										let addr_value =
											&unsafe { self.btree.get_at(addr) }.unwrap().value;
										if *addr_value == value {
											key.add(&item_before);
											self.set_item_key(addr, next_addr, key);
										} else {
											let old_value = self
												.set_item(addr, next_addr, key, value)
												.removed_value;
											self.insert_item(addr, item_before, old_value);
										}
									}
								}

								break;
							}
							Some(ProductArg::Subject(_)) | None => {
								// SAFETY: `addr` is a valid address in this tree. The
								// closure is only ever called with `prev_addr` values
								// returned by `previous_item_address` itself.
								let prev = unsafe { self.btree.previous_item_address(addr) }
									.filter(|&prev_addr| {
										// SAFETY: `prev_addr` was just returned by
										// `previous_item_address` as a valid address.
										unsafe { self.btree.get_at(prev_addr) }
											.unwrap()
											.key
											.connected_to(&key)
									});

								match prev {
									Some(prev_addr) => {
										// We can move one to the previous item.
										let (prev_addr, addr) = if removed_item_value.is_none() {
											self.remove_item(addr)
										} else {
											(prev_addr, Some(addr))
										};

										(prev_addr, addr)
									}
									None => {
										// There is no previous item, we must get it done now.
										if removed_item_value.is_some() {
											self.insert_item(addr, key, value);
										} else {
											self.set_item(addr, next_addr, key, value);
										}

										break;
									}
								}
							}
						}
					};

					addr = prev_addr;
					next_addr = prev_next_addr;
				}
			}
			Err(addr) => {
				// case (G)
				// SAFETY: `addr` is `None` only if the tree is empty, which
				// `insert_at` handles.
				unsafe { self.btree.insert_at(addr, Item::new(key, value)) };
			}
		}
	}

	/// Remove a key.
	///
	/// # Complexity
	///
	/// `O((k + 1) log n)`, where `n` is the number of ranges in the map
	/// ([`Self::range_count`]) and `k` is the number of existing ranges that
	/// intersect `key`. Each intersecting range costs one `O(log n)` B-tree
	/// operation (split, shrink or removal), on top of the initial `O(log n)`
	/// lookup. Usually `O(log n)`; worst case (`key` intersects every stored
	/// range) is `O(n log n)`.
	pub fn remove<R: AsRange<Item = K>>(&mut self, key: R)
	where
		K: Clone + PartialEnum + Measure,
		V: Clone,
	{
		let key = AnyRange::from(key);
		if let Ok(mut addr) = self.address_of(&key, false) {
			loop {
				// SAFETY: `addr` is a valid address in this tree.
				let intersects = unsafe { self.btree.get_at(addr) }
					.map(|item| item.key.intersects(&key))
					.unwrap_or(false);

				if intersects {
					// SAFETY: `addr` is a valid address in this tree.
					let difference = unsafe { self.btree.get_at(addr) }
						.unwrap()
						.key
						.without(&key);
					match difference {
						Difference::Split(left, right) => {
							let left = left.cloned();
							let right = right.cloned();

							let right_value = {
								// SAFETY: `addr` is a valid address in this tree.
								let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
								item.key = right;
								item.value.clone()
							};
							// SAFETY: `addr` is a valid address in this tree.
							unsafe {
								self.btree
									.insert_at(Some(addr), Item::new(left, right_value))
							};
							break; // no need to go further, the removed range was totaly included in this one.
						}
						Difference::Before(left, _) => {
							let left = left.cloned();
							// SAFETY: `addr` is a valid address in this tree.
							let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
							item.key = left;
							break; // no need to go further, the removed range does not intersect anything below this range.
						}
						Difference::After(right, _) => {
							let right = right.cloned();
							// SAFETY: `addr` is a valid address in this tree.
							let item = unsafe { self.btree.get_mut_at(addr) }.unwrap();
							item.key = right;
						}
						Difference::Empty => {
							// SAFETY: `addr` is a valid address in this tree.
							let (_, next_addr) = unsafe { self.btree.remove_at(addr) }.unwrap();
							match next_addr {
								Some(next_addr) => addr = next_addr,
								None => break,
							}
						}
					}

					// SAFETY: `addr` is a valid address in this tree.
					match unsafe { self.btree.previous_item_address(addr) } {
						Some(prev_addr) => addr = prev_addr,
						None => break,
					}
				} else {
					break;
				}
			}
		}
	}
}

struct SetItem<N, V> {
	new_addr: Address<N>,
	new_next_addr: Option<Address<N>>,
	removed_value: V,
}

impl<N, V> SetItem<N, V> {
	fn new(new_addr: Address<N>, new_next_addr: Option<Address<N>>, removed_value: V) -> Self {
		SetItem {
			new_addr,
			new_next_addr,
			removed_value,
		}
	}
}

impl<N, V> From<SetItem<N, V>> for (Address<N>, Option<Address<N>>, V) {
	fn from(item: SetItem<N, V>) -> Self {
		(item.new_addr, item.new_next_addr, item.removed_value)
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> IntoIterator for RangeMap<K, V, C> {
	type Item = (AnyRange<K>, V);
	type IntoIter = IntoIter<K, V, C>;

	fn into_iter(self) -> Self::IntoIter {
		IntoIter {
			inner: self.btree.into_iter(),
		}
	}
}

/// Iterator over the entries of a `RangeMap`.
pub struct Iter<'a, K, V, C: Storage<Item<AnyRange<K>, V>>> {
	inner: raw_btree::Iter<'a, Item<AnyRange<K>, V>, C>,
}

impl<'a, K, V, C: Storage<Item<AnyRange<K>, V>>> Iterator for Iter<'a, K, V, C> {
	type Item = (&'a AnyRange<K>, &'a V);

	fn next(&mut self) -> Option<Self::Item> {
		self.inner.next().map(Item::as_pair)
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.inner.size_hint()
	}
}

impl<'a, K, V, C: Storage<Item<AnyRange<K>, V>>> DoubleEndedIterator for Iter<'a, K, V, C> {
	fn next_back(&mut self) -> Option<Self::Item> {
		self.inner.next_back().map(Item::as_pair)
	}
}

impl<'a, K, V, C: Storage<Item<AnyRange<K>, V>>> ExactSizeIterator for Iter<'a, K, V, C> {}

impl<'a, K, V, C: Storage<Item<AnyRange<K>, V>>> Clone for Iter<'a, K, V, C> {
	fn clone(&self) -> Self {
		Iter { inner: self.inner }
	}
}

/// Consuming iterator over the entries of a `RangeMap`.
pub struct IntoIter<K, V, C: Storage<Item<AnyRange<K>, V>>> {
	inner: raw_btree::IntoIter<Item<AnyRange<K>, V>, C>,
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> Iterator for IntoIter<K, V, C> {
	type Item = (AnyRange<K>, V);

	fn next(&mut self) -> Option<Self::Item> {
		self.inner.next().map(Item::into_pair)
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.inner.size_hint()
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> DoubleEndedIterator for IntoIter<K, V, C> {
	fn next_back(&mut self) -> Option<Self::Item> {
		self.inner.next_back().map(Item::into_pair)
	}
}

impl<K, V, C: Storage<Item<AnyRange<K>, V>>> ExactSizeIterator for IntoIter<K, V, C> {}

/// Iterator over the gaps (unbound keys) of a `RangeMap`.
pub struct Gaps<'a, K, V, C: Storage<Item<AnyRange<K>, V>>> {
	inner: Iter<'a, K, V, C>,
	prev: Option<std::ops::Bound<&'a K>>,
	done: bool,
}

impl<'a, K: Measure + PartialEnum, V, C: Storage<Item<AnyRange<K>, V>>> Iterator
	for Gaps<'a, K, V, C>
{
	type Item = AnyRange<&'a K>;

	fn next(&mut self) -> Option<Self::Item> {
		use std::ops::{Bound, RangeBounds};

		if self.done {
			None
		} else {
			loop {
				match self.inner.next() {
					Some((range, _)) => {
						let start = match self.prev.take() {
							Some(bound) => bound,
							None => Bound::Unbounded,
						};

						self.prev = match range.end_bound() {
							Bound::Unbounded => {
								self.done = true;
								None
							}
							Bound::Included(t) => Some(Bound::Excluded(t)),
							Bound::Excluded(t) => Some(Bound::Included(t)),
						};

						let end = match range.start_bound() {
							Bound::Unbounded => continue,
							Bound::Included(t) => Bound::Excluded(t),
							Bound::Excluded(t) => Bound::Included(t),
						};

						let gap = AnyRange { start, end };

						if !gap.ref_is_empty() {
							break Some(gap);
						}
					}
					None => {
						self.done = true;
						let start = self.prev.take();
						match start {
							Some(bound) => {
								let gap = AnyRange {
									start: bound,
									end: Bound::Unbounded,
								};

								break if gap.ref_is_empty() { None } else { Some(gap) };
							}
							None => {
								break Some(AnyRange {
									start: Bound::Unbounded,
									end: Bound::Unbounded,
								});
							}
						}
					}
				}
			}
		}
	}
}

/// Search for the index of the greatest item less/below or equal/including the given element.
///
/// If `connected` is `true`, then it will search for the greatest item less/below or equal/including **or connected to** the given element.
pub fn binary_search<T: Measure + PartialEnum, U, V, I: AsRef<Item<AnyRange<T>, V>>>(
	items: &[I],
	element: &U,
	connected: bool,
) -> Option<usize>
where
	U: RangePartialOrd<T>,
{
	if items.is_empty()
		|| element
			.range_partial_cmp(&items[0].as_ref().key)
			.unwrap_or(RangeOrdering::Before(false))
			.is_before(connected)
	{
		None
	} else {
		let mut i = 0;
		let mut j = items.len() - 1;

		if !element
			.range_partial_cmp(&items[j].as_ref().key)
			.unwrap_or(RangeOrdering::After(false))
			.is_before(connected)
		{
			return Some(j);
		}

		// invariants:
		// vec[i].as_ref().key() < range
		// vec[j].as_ref().key() >= range
		// j > i

		while j - i > 1 {
			let k = (i + j) / 2;

			if let Some(ord) = element.range_partial_cmp(&items[k].as_ref().key) {
				if ord.is_before(connected) {
					j = k;
				} else {
					i = k;
				}
			} else {
				return None; // FIXME: that's bad. Maybe we should expect a total order.
			}
		}

		Some(i)
	}
}

#[cfg(test)]
mod tests {
	use std::{collections::HashSet, ops::Bound};

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

	#[test]
	fn insert() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert('+', 0);
		map.insert('-', 1);
		map.insert('0'..='9', 2);
		map.insert('.', 3);

		assert_eq!(*map.get('.').unwrap(), 3)
	}

	#[test]
	fn insert_around() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert(' ', 0);
		map.insert('#', 1);
		map.insert('e', 2);
		map.insert('%', 3);
		map.insert('A'..='Z', 4);
		map.insert('a'..='z', 5);

		assert!(map.get('a').is_some())
	}

	#[test]
	fn update_connected_after() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert('+', 0);
		map.insert('-', 1);
		map.insert('0'..='9', 2);
		map.update('.', |binding| {
			assert!(binding.is_none());
			Some(3)
		});

		assert_eq!(*map.get('.').unwrap(), 3)
	}

	#[test]
	fn update_singleton() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert('*', 0);
		map.update('*', |_| Some(1));

		assert_eq!(map.iter().count(), 1);
		assert_eq!(map.get('*'), Some(&1))
	}

	#[test]
	fn update_connected_before() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert('+', 0);
		map.insert('.', 1);
		map.insert('0'..='9', 2);
		map.update('-', |binding| {
			assert!(binding.is_none());
			Some(3)
		});

		assert_eq!(map.iter().count(), 4);
		assert_eq!(*map.get('-').unwrap(), 3)
	}

	#[test]
	fn update_around() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert('e', 0);
		map.update('a'..='z', |_| Some(1));

		assert_eq!(map.iter().count(), 1);
		assert_eq!(map.get('a'), Some(&1))
	}

	#[test]
	fn update_stress() {
		let ranges = [
			// 'A'..='Z',
			// 'a'..='z',
			// '0'..='9',
			// '-'..='-',
			// '.'..='.',
			// '_'..='_',
			// '~'..='~',
			// '%'..='%',
			// '!'..='!',
			// '$'..='$',
			// '&'..='&',
			// '\''..='\'',
			// '('..='(',
			// ')'..=')',
			// '*'..='*',
			// '+'..='+',
			','..=',',
			';'..=';',
			'='..='=',
			':'..=':',
			// '@'..='@',
			// '['..='[',
			// '0'..='9',
			// '1'..='9',
			// '1'..='1',
			// '2'..='2',
			// '2'..='2',
			// 'A'..='Z',
			// 'a'..='z',
			// '0'..='9',
			// '-'..='-',
			// '.'..='.',
			// '_'..='_',
			// '~'..='~',
			// '%'..='%',
			// '!'..='!',
			// '$'..='$',
			// '&'..='&',
			'\''..='\'',
			'('..='(',
			')'..=')',
			'*'..='*',
			'+'..='+',
			// ','..=',',

			// ';'..=';',
			// '='..='=',
			// ':'..=':',
			// '/'..='/',
			// '?'..='?',
			// '#'..='#'
		];

		let mut map: crate::RangeMap<char, Vec<usize>> = crate::RangeMap::new();

		for (i, range) in ranges.into_iter().enumerate() {
			map.update(range, |current| {
				let mut list = current.cloned().unwrap_or_default();
				list.push(i);
				Some(list)
			});
		}

		eprintln!("before: {map:?}");

		map.update(','..=',', |current| {
			let mut list = current.cloned().unwrap_or_default();
			list.push(9);
			Some(list)
		});

		eprintln!("after: {map:?}");

		let mut found_ranges = HashSet::new();
		for (range, _) in map.iter() {
			eprintln!("looking for range: {range:?}");
			assert!(found_ranges.insert(range))
		}
	}

	#[test]
	fn update_stress2() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert('+'..='+', 0);
		map.insert(AnyRange::new(Bound::Excluded('+'), Bound::Included(',')), 1);
		map.update(','..=',', |_| Some(2));

		let mut found_ranges = HashSet::new();
		for (range, _) in map.iter() {
			eprintln!("looking for range: {range:?}");
			assert!(found_ranges.insert(range))
		}
	}

	#[test]
	fn update_test() {
		let mut map: crate::RangeMap<char, usize> = crate::RangeMap::new();

		map.insert('0'..='9', 0);
		map.insert(
			AnyRange::new(Bound::Excluded('\''), Bound::Included('(')),
			1,
		);
		map.insert(AnyRange::new(Bound::Excluded('('), Bound::Included(')')), 2);
		map.insert(AnyRange::new(Bound::Excluded(')'), Bound::Included('*')), 3);
		map.insert('+', 4);
		map.insert(',', 5);
		map.insert('-', 6);
		map.insert('.', 7);
		map.insert('/', 8);

		assert_eq!(map.range_count(), 9);
		assert_eq!(map.iter().count(), 9);

		map.update(
			AnyRange::new(Bound::Excluded('\''), Bound::Included('(')),
			|_| Some(10),
		);
		map.update(
			AnyRange::new(Bound::Excluded('('), Bound::Included(')')),
			|_| Some(11),
		);
		map.update(
			AnyRange::new(Bound::Excluded(')'), Bound::Included('*')),
			|_| Some(12),
		);

		assert_eq!(map.range_count(), 9);
		assert_eq!(map.iter().count(), 9);

		// let mut ranges = map.iter();
		// let (a, _) = ranges.next().unwrap();
		// assert_eq!(a.first(), Some('('));
		// assert_eq!(a.last(), Some(')'));

		// let (b, _) = ranges.next().unwrap();
		// assert_eq!(b.first(), Some('*'));
		// assert_eq!(b.last(), Some('*'));

		// let (c, _) = ranges.next().unwrap();
		// assert_eq!(c.first(), Some('+'));
		// assert_eq!(c.last(), Some('9'));
	}

	/// Reproduces an overlapping-ranges bug found by folding a single wide
	/// range (`'0'..='9'`) into a map that already has several separate
	/// single-character entries (`'1'..='1'`, `'2'..='2'`, ..., `'9'..='9'`),
	/// each mapped to a *distinct* value, via repeated calls to `update`.
	/// `update` merges values on overlap. The resulting map must always be a
	/// proper partition of the key space: no two entries may overlap.
	#[test]
	fn update_digit_fanout() {
		use std::collections::BTreeSet;

		let mut map: crate::RangeMap<char, BTreeSet<i32>> = crate::RangeMap::new();

		// Simulate the `DIGIT` rule: one wide range, single target `0`.
		map.update('0'..='9', |current: Option<&BTreeSet<i32>>| {
			let mut set = current.cloned().unwrap_or_default();
			set.insert(0);
			Some(set)
		});

		// Simulate the `NZDIGIT` rule's fan-out: one single-char range per
		// literal, each with a *distinct* target id (as would arise from
		// Thompson's construction of an alternation of literals).
		for (i, c) in ('1'..='9').enumerate() {
			let id = 100 + i as i32;
			map.update(c..=c, move |current: Option<&BTreeSet<i32>>| {
				let mut set = current.cloned().unwrap_or_default();
				set.insert(id);
				Some(set)
			});
		}

		for (range, set) in map.iter() {
			eprintln!("{range:?} -> {set:?}");
		}

		let entries: Vec<_> = map.iter().collect();
		for i in 0..entries.len() {
			for j in (i + 1)..entries.len() {
				assert!(
					!entries[i].0.intersects(entries[j].0),
					"overlapping ranges: {:?} and {:?}",
					entries[i].0,
					entries[j].0
				);
			}
		}

		// Every digit must be covered by exactly the union of `{0}` and
		// whichever `NZDIGIT` literal (if any) matches it.
		for c in '0'..='9' {
			let set = map.get(c).unwrap_or_else(|| panic!("no entry for {c:?}"));
			assert!(
				set.contains(&0),
				"digit {c:?} should always contain 0, got {set:?}"
			);
		}
	}

	/// Same scenario as [`update_digit_fanout`], but with the single-character
	/// updates applied *before* the wide range update.
	///
	/// This used to reproduce a bug in `RangeMap`'s `address_in`/`offset_in`
	/// binary search: once *9* (but not fewer - see the loop below)
	/// pre-existing, adjacent, distinctly-valued single-item ranges existed
	/// in the map, the tree grew an internal node (the Knuth order is
	/// `M = 8`), and `address_of` would stop as soon as it found a match on
	/// an *internal separator* item instead of also checking that
	/// separator's right subtree for an even further-right match. Folding a
	/// wider range on top of all 9 items via `update` would then only walk
	/// backward from that separator, silently skipping every item to its
	/// right (`'6'..='6'`, `'7'..='7'`, `'8'..='8'`, `'9'..='9'`), leaving
	/// them orphaned while a bogus `'5'..='9'` entry appeared over them.
	fn digit_fanout_reversed_with_count(n: u32) {
		use std::collections::BTreeSet;

		let mut map: crate::RangeMap<char, BTreeSet<i32>> = crate::RangeMap::new();

		let digits: Vec<char> = ('1'..='9').take(n as usize).collect();

		for (i, &c) in digits.iter().enumerate() {
			let id = 100 + i as i32;
			map.update(c..=c, move |current: Option<&BTreeSet<i32>>| {
				let mut set = current.cloned().unwrap_or_default();
				set.insert(id);
				Some(set)
			});
		}

		let last = *digits.last().unwrap();
		map.update('0'..=last, |current: Option<&BTreeSet<i32>>| {
			let mut set = current.cloned().unwrap_or_default();
			set.insert(0);
			Some(set)
		});

		println!("-- n = {n} --");
		for (range, set) in map.iter() {
			println!("{range:?} -> {set:?}");
		}

		let entries: Vec<_> = map.iter().collect();
		for i in 0..entries.len() {
			for j in (i + 1)..entries.len() {
				assert!(
					!entries[i].0.intersects(entries[j].0),
					"n={n}: overlapping ranges: {:?} and {:?}",
					entries[i].0,
					entries[j].0
				);
			}
		}
	}

	/// Regression test for the overlapping-ranges bug that motivated the
	/// switch from `btree-slab` to `raw-btree`: see
	/// [`digit_fanout_reversed_with_count`]. This now passes at every `n`.
	#[test]
	fn update_digit_fanout_reversed() {
		for n in 1..=9 {
			digit_fanout_reversed_with_count(n);
		}
	}
}
