use super::Node;
use crate::{
	range::{Difference, ProductArg},
	AnyRange, AsRange, IntoRange, RangeOrdering, RangePartialOrd,
};
use btree_slab::generic::{
	map::{BTreeExt, BTreeExtMut, BTreeMap},
	node::{Address, Item, Offset},
};
use cc_traits::{SimpleCollectionMut, SimpleCollectionRef, Slab, SlabMut};
use range_traits::{Bounded, Measure, PartialEnum};
use std::{
	cmp::{Ord, Ordering, PartialOrd},
	fmt,
	hash::{Hash, Hasher},
};

/// Range map.
#[derive(Clone)]
pub struct RangeMap<K, V, C> {
	btree: BTreeMap<AnyRange<K>, V, C>,
}

impl<K, V, C> RangeMap<K, V, C> {
	/// Create a new empty map.
	pub fn new() -> RangeMap<K, V, C>
	where
		C: Default,
	{
		RangeMap {
			btree: BTreeMap::new(),
		}
	}
}

impl<K, T, C: Default> Default for RangeMap<K, T, C> {
	fn default() -> Self {
		Self::new()
	}
}

impl<K, V, C: Slab<Node<AnyRange<K>, V>>> RangeMap<K, V, C>
where
	C: SimpleCollectionRef,
{
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

	fn address_of<T>(&self, key: &T, connected: bool) -> Result<Address, Address>
	where
		K: PartialEnum + Measure,
		T: RangePartialOrd<K>,
	{
		if connected {
			if let Ok(addr) = self.address_of(key, false) {
				return Ok(addr);
			}
		}

		match self.btree.root_id() {
			Some(id) => self.address_in(id, key, connected),
			None => Err(Address::nowhere()),
		}
	}

	fn address_in<T>(&self, mut id: usize, key: &T, connected: bool) -> Result<Address, Address>
	where
		K: PartialEnum + Measure,
		T: RangePartialOrd<K>,
	{
		loop {
			match self.offset_in(id, key, connected) {
				Ok(offset) => return Ok(Address::new(id, offset)),
				Err((offset, None)) => return Err(Address::new(id, offset.into())),
				Err((_, Some(child_id))) => {
					id = child_id;
				}
			}
		}
	}

	fn offset_in<T>(
		&self,
		id: usize,
		key: &T,
		connected: bool,
	) -> Result<Offset, (usize, Option<usize>)>
	where
		K: PartialEnum + Measure,
		T: RangePartialOrd<K>,
	{
		match self.btree.node(id) {
			Node::Internal(node) => {
				let branches = node.branches();
				match binary_search(branches, key, connected) {
					Some(i) => {
						let b = &branches[i];
						if key
							.range_partial_cmp(b.item.key())
							.unwrap_or(RangeOrdering::After(false))
							.matches(connected)
						{
							Ok(i.into())
						} else {
							Err((i + 1, Some(b.child)))
						}
					}
					None => Err((0, Some(node.first_child_id()))),
				}
			}
			Node::Leaf(leaf) => {
				let items = leaf.items();
				match binary_search(items, key, connected) {
					Some(i) => {
						let item = &items[i];
						let ord = key
							.range_partial_cmp(item.key())
							.unwrap_or(RangeOrdering::After(false));
						if ord.matches(connected) {
							Ok(i.into())
						} else {
							Err((i + 1, None))
						}
					}
					None => Err((0, None)),
				}
			}
		}
	}

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

	pub fn contains_key(&self, key: K) -> bool
	where
		K: PartialEnum + RangePartialOrd + Measure,
	{
		self.address_of(&key, false).is_ok()
	}

	pub fn get(&self, key: K) -> Option<&V>
	where
		K: PartialEnum + RangePartialOrd + Measure,
	{
		match self.address_of(&key, false) {
			Ok(addr) => Some(self.btree.item(addr).unwrap().value()),
			Err(_) => None,
		}
	}

	pub fn iter(&self) -> Iter<K, V, C> {
		self.btree.iter()
	}

	/// Returns an iterator over the gaps (unbounded keys) of the map.
	pub fn gaps(&self) -> Gaps<K, V, C> {
		Gaps {
			inner: self.btree.iter(),
			prev: None,
			done: false,
		}
	}
}

impl<K: fmt::Debug, V: fmt::Debug, C: Slab<Node<AnyRange<K>, V>>> fmt::Debug for RangeMap<K, V, C>
where
	C: SimpleCollectionRef,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{")?;

		for (range, value) in self {
			write!(f, "{:?}=>{:?}", range, value)?
		}

		write!(f, "}}")
	}
}

impl<K, L, V, W, C: Slab<Node<AnyRange<K>, V>>, D: Slab<Node<AnyRange<L>, W>>>
	PartialEq<RangeMap<L, W, D>> for RangeMap<K, V, C>
where
	L: Measure<K> + PartialOrd<K> + PartialEnum,
	K: PartialEnum,
	W: PartialEq<V>,
	C: SimpleCollectionRef,
	D: SimpleCollectionRef,
{
	fn eq(&self, other: &RangeMap<L, W, D>) -> bool {
		self.btree == other.btree
	}
}

impl<K, V, C: Slab<Node<AnyRange<K>, V>>> Eq for RangeMap<K, V, C>
where
	K: Measure + PartialEnum + Ord,
	V: Eq,
	C: SimpleCollectionRef,
{
}

impl<K, L, V, W, C: Slab<Node<AnyRange<K>, V>>, D: Slab<Node<AnyRange<L>, W>>>
	PartialOrd<RangeMap<L, W, D>> for RangeMap<K, V, C>
where
	L: Measure<K> + PartialOrd<K> + PartialEnum,
	K: PartialEnum,
	W: PartialOrd<V>,
	C: SimpleCollectionRef,
	D: SimpleCollectionRef,
{
	fn partial_cmp(&self, other: &RangeMap<L, W, D>) -> Option<Ordering> {
		self.btree.partial_cmp(&other.btree)
	}
}

impl<K, V, C: Slab<Node<AnyRange<K>, V>>> Ord for RangeMap<K, V, C>
where
	K: Measure + PartialEnum + Ord,
	V: Ord,
	C: SimpleCollectionRef,
{
	fn cmp(&self, other: &Self) -> Ordering {
		self.btree.cmp(&other.btree)
	}
}

impl<K, V, C: Slab<Node<AnyRange<K>, V>>> Hash for RangeMap<K, V, C>
where
	K: Hash + PartialEnum,
	V: Hash,
	C: SimpleCollectionRef,
{
	fn hash<H: Hasher>(&self, h: &mut H) {
		for range in self {
			range.hash(h)
		}
	}
}

impl<'a, K, V, C: Slab<Node<AnyRange<K>, V>>> IntoIterator for &'a RangeMap<K, V, C>
where
	C: SimpleCollectionRef,
{
	type Item = (&'a AnyRange<K>, &'a V);
	type IntoIter = Iter<'a, K, V, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<K, V, C: SlabMut<Node<AnyRange<K>, V>>> RangeMap<K, V, C>
where
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	fn merge_forward(&mut self, addr: Address, next_addr: Option<Address>)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		if let Some(next_addr) = next_addr {
			let item = self.btree.item(addr).unwrap();
			let next_item = self.btree.item(next_addr).unwrap();
			if item.key().connected_to(next_item.key()) && item.value() == next_item.value() {
				let (removed_item, non_normalized_new_addr) = self.btree.remove_at(addr).unwrap();
				let new_addr = self.btree.normalize(non_normalized_new_addr).unwrap();
				let item = self.btree.item_mut(new_addr).unwrap();
				item.key_mut().add(removed_item.key());
			}
		}
	}

	fn set_item_key(
		&mut self,
		addr: Address,
		next_addr: Option<Address>,
		new_key: AnyRange<K>,
	) -> (Address, Option<Address>)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		if let Some(next_addr) = next_addr {
			let next_item = self.btree.item(next_addr).unwrap();
			if new_key.connected_to(next_item.key())
				&& next_item.value() == self.btree.item(addr).unwrap().value()
			{
				// Merge with the next item.
				let (_, non_normalized_new_addr) = self.btree.remove_at(addr).unwrap();
				let new_addr = self.btree.normalize(non_normalized_new_addr).unwrap();
				let item = self.btree.item_mut(new_addr).unwrap();
				item.key_mut().add(&new_key);

				return (new_addr, self.btree.next_item_address(new_addr));
			}
		}

		let item = self.btree.item_mut(addr).unwrap();
		*item.key_mut() = new_key;
		(addr, next_addr)
	}

	fn set_item(
		&mut self,
		addr: Address,
		next_addr: Option<Address>,
		new_key: AnyRange<K>,
		new_value: V,
	) -> (Address, Option<Address>, V)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		if let Some(next_addr) = next_addr {
			let next_item = self.btree.item(next_addr).unwrap();
			if new_key.connected_to(next_item.key()) && *next_item.value() == new_value {
				// Merge with the next item.
				let (removed_item, non_normalized_new_addr) = self.btree.remove_at(addr).unwrap();
				let new_addr = self.btree.normalize(non_normalized_new_addr).unwrap();
				let item = self.btree.item_mut(new_addr).unwrap();
				item.key_mut().add(&new_key);

				return (
					new_addr,
					self.btree.next_item_address(new_addr),
					removed_item.into_value(),
				);
			}
		}

		let item = self.btree.item_mut(addr).unwrap();
		let removed_value = item.set_value(new_value);
		*item.key_mut() = new_key;
		(addr, next_addr, removed_value)
	}

	fn insert_item(
		&mut self,
		addr: Address,
		key: AnyRange<K>,
		value: V,
	) -> (Address, Option<Address>)
	where
		K: Clone + PartialEnum + Measure,
		V: PartialEq,
	{
		let next_item = self.btree.item(addr).unwrap();
		if key.connected_to(next_item.key()) && *next_item.value() == value {
			// Merge with the next item.
			let item = self.btree.item_mut(addr).unwrap();
			item.key_mut().add(&key);

			return (addr, self.btree.next_item_address(addr));
		}

		let new_addr = self.btree.insert_at(addr, Item::new(key, value));
		(new_addr, self.btree.next_item_address(new_addr))
	}

	fn remove_item(&mut self, addr: Address) -> (Address, Option<Address>) {
		let (_, non_normalized_addr) = self.btree.remove_at(addr).unwrap();
		let new_addr = self
			.btree
			.previous_item_address(non_normalized_addr)
			.unwrap();
		(new_addr, self.btree.normalize(non_normalized_addr))
	}

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
				let mut next_addr = self.btree.next_item_address(addr);

				loop {
					let (prev_addr, prev_next_addr) = {
						let product = key.product(self.btree.item(addr).unwrap().key()).cloned();

						let mut removed_item_value = None;

						let (addr, next_addr) = match product.after {
							Some(ProductArg::Subject(key_after)) => {
								match f(None) {
									Some(value) => {
										let (new_addr, new_next_addr, removed_value) =
											self.set_item(addr, next_addr, key_after, value);
										removed_item_value = Some(removed_value);
										(new_addr, new_next_addr)
									}
									None => (addr, next_addr), // we wait the last minute to remove the item.
								}
							}
							Some(ProductArg::Object(item_after)) => {
								let item = self.btree.item_mut(addr).unwrap();
								item.set_key(item_after);
								removed_item_value = Some(item.value().clone());
								(addr, next_addr)
							}
							None => (addr, next_addr), // we wait the last minute to remove the item.
						};

						let (addr, next_addr) = match product.intersection {
							Some(intersection) => {
								let new_value = match removed_item_value.as_ref() {
									Some(value) => f(Some(value)),
									None => f(Some(self.btree.item(addr).unwrap().value())),
								};

								match new_value {
									Some(new_value) => {
										if removed_item_value.is_some() {
											let (new_addr, new_next_addr) =
												self.insert_item(addr, intersection, new_value);
											(new_addr, new_next_addr)
										} else {
											let (new_addr, new_next_addr, removed_value) = self
												.set_item(addr, next_addr, intersection, new_value);
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
								match self.btree.previous_item_address(addr) {
									Some(prev_addr)
										if self
											.btree
											.item(prev_addr)
											.unwrap()
											.key()
											.connected_to(&key_before) =>
									{
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
									_ => {
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
													self.btree.remove_at(addr); // finally remove the item.
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
								match self.btree.previous_item_address(addr) {
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
											self.btree.remove_at(addr).unwrap();
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
					self.btree.insert_at(addr, Item::new(key, new_value));
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
				self.btree.insert_at(addr, Item::new(key, value));
				Ok(())
			}
		}
	}

	/// Insert a new key-value binding.
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
				let mut next_addr = self.btree.next_item_address(addr);

				loop {
					let (prev_addr, prev_next_addr) = {
						let product = key.product(self.btree.item(addr).unwrap().key()).cloned();

						let mut removed_item_value = None;

						if let Some(ProductArg::Object(item_after)) = product.after {
							let item = self.btree.item_mut(addr).unwrap();
							item.set_key(item_after);
							removed_item_value = Some(item.value().clone());
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
										if *self.btree.item(addr).unwrap().value() == value {
											key.add(&item_before);
											self.set_item_key(addr, next_addr, key);
										} else {
											let (_, _, old_value) =
												self.set_item(addr, next_addr, key, value);
											self.insert_item(addr, item_before, old_value);
										}
									}
								}

								break;
							}
							Some(ProductArg::Subject(_)) | None => {
								match self.btree.previous_item_address(addr) {
									Some(prev_addr)
										if self
											.btree
											.item(prev_addr)
											.unwrap()
											.key()
											.connected_to(&key) =>
									{
										// We can move one to the previous item.
										let (prev_addr, addr) = if removed_item_value.is_none() {
											self.remove_item(addr)
										} else {
											(prev_addr, Some(addr))
										};

										(prev_addr, addr)
									}
									_ => {
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
				self.btree.insert_at(addr, Item::new(key, value));
			}
		}
	}

	/// Remove a key.
	pub fn remove<R: AsRange<Item = K>>(&mut self, key: R)
	where
		K: Clone + PartialEnum + Measure,
		V: Clone,
	{
		let key = AnyRange::from(key);
		if let Ok(mut addr) = self.address_of(&key, false) {
			loop {
				if self
					.btree
					.item(addr)
					.map(|item| item.key().intersects(&key))
					.unwrap_or(false)
				{
					match self.btree.item(addr).unwrap().key().without(&key) {
						Difference::Split(left, right) => {
							let left = left.cloned();
							let right = right.cloned();

							let right_value = {
								let item = self.btree.item_mut(addr).unwrap();
								*item.key_mut() = right;
								item.value().clone()
							};
							self.btree.insert_at(addr, Item::new(left, right_value));
							break; // no need to go further, the removed range was totaly included in this one.
						}
						Difference::Before(left, _) => {
							let left = left.cloned();
							let item = self.btree.item_mut(addr).unwrap();
							*item.key_mut() = left;
							break; // no need to go further, the removed range does not intersect anything below this range.
						}
						Difference::After(right, _) => {
							let right = right.cloned();
							let item = self.btree.item_mut(addr).unwrap();
							*item.key_mut() = right;
						}
						Difference::Empty => {
							let (_, next_addr) = self.btree.remove_at(addr).unwrap();
							addr = next_addr
						}
					}

					match self.btree.previous_item_address(addr) {
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

impl<K, V, C: SlabMut<Node<AnyRange<K>, V>>> IntoIterator for RangeMap<K, V, C>
where
	C: SimpleCollectionRef,
	C: SimpleCollectionMut,
{
	type Item = (AnyRange<K>, V);
	type IntoIter = IntoIter<K, V, C>;

	fn into_iter(self) -> Self::IntoIter {
		self.btree.into_iter()
	}
}

pub type Iter<'a, K, V, C> = btree_slab::generic::map::Iter<'a, AnyRange<K>, V, C>;
pub type IntoIter<K, V, C> = btree_slab::generic::map::IntoIter<AnyRange<K>, V, C>;

/// Iterator over the gaps (unbound keys) of a `RangeMap`.
pub struct Gaps<'a, K, V, C> {
	inner: Iter<'a, K, V, C>,
	prev: Option<std::ops::Bound<&'a K>>,
	done: bool,
}

impl<'a, K: Measure + PartialEnum, V, C: Slab<Node<AnyRange<K>, V>>> Iterator for Gaps<'a, K, V, C>
where
	C: SimpleCollectionRef,
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
								})
							}
						}
					}
				}
			}
		}
	}
}

/// Search for the index of the gratest item less/below or equal/including the given element.
///
/// If `connected` is `true`, then it will search for the gratest item less/below or equal/including **or connected to** the given element.
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
			.range_partial_cmp(items[0].as_ref().key())
			.unwrap_or(RangeOrdering::Before(false))
			.is_before(connected)
	{
		None
	} else {
		let mut i = 0;
		let mut j = items.len() - 1;

		if !element
			.range_partial_cmp(items[j].as_ref().key())
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

			if let Some(ord) = element.range_partial_cmp(items[k].as_ref().key()) {
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
}
