use std::marker::PhantomData;

use btree_slab::generic::Node;
use cc_traits::{SimpleCollectionMut, SimpleCollectionRef, Slab, SlabMut};
use range_traits::{Measure, PartialEnum};
use serde::{
	de::Error,
	ser::{SerializeMap, SerializeSeq, SerializeTuple},
	Deserialize, Serialize,
};

use crate::{generic, AnyRange};

impl<T: Serialize> Serialize for AnyRange<T> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		let mut t = serializer.serialize_tuple(2)?;
		t.serialize_element(&self.start)?;
		t.serialize_element(&self.end)?;
		t.end()
	}
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for AnyRange<T> {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		struct Visitor<T>(PhantomData<T>);

		impl<'de, T: Deserialize<'de>> serde::de::Visitor<'de> for Visitor<T> {
			type Value = AnyRange<T>;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				write!(formatter, "any range")
			}

			fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::SeqAccess<'de>,
			{
				let start = seq
					.next_element()?
					.ok_or_else(|| A::Error::custom("missing start bound"))?;
				let end = seq
					.next_element()?
					.ok_or_else(|| A::Error::custom("missing end bound"))?;
				Ok(AnyRange { start, end })
			}
		}

		deserializer.deserialize_tuple(2, Visitor(PhantomData))
	}
}

impl<T: Serialize, C: SimpleCollectionRef + Slab<Node<AnyRange<T>, ()>>> Serialize
	for generic::RangeSet<T, C>
{
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		let mut seq = serializer.serialize_seq(Some(self.range_count()))?;

		for range in self {
			seq.serialize_element(range)?;
		}

		seq.end()
	}
}

impl<
		'de,
		T: Clone + PartialEnum + Measure + Deserialize<'de>,
		C: Default + SimpleCollectionRef + SimpleCollectionMut + SlabMut<Node<AnyRange<T>, ()>>,
	> Deserialize<'de> for generic::RangeSet<T, C>
{
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		struct Visitor<T, C>(PhantomData<(T, C)>);

		impl<
				'de,
				T: Clone + PartialEnum + Measure + Deserialize<'de>,
				C: Default
					+ SimpleCollectionRef
					+ SimpleCollectionMut
					+ SlabMut<Node<AnyRange<T>, ()>>,
			> serde::de::Visitor<'de> for Visitor<T, C>
		{
			type Value = generic::RangeSet<T, C>;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				write!(formatter, "a range set")
			}

			fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::SeqAccess<'de>,
			{
				let mut result = generic::RangeSet::new();

				while let Some(range) = seq.next_element::<AnyRange<T>>()? {
					result.insert(range);
				}

				Ok(result)
			}
		}

		deserializer.deserialize_seq(Visitor(PhantomData))
	}
}

impl<K: Serialize, V: Serialize, C: SimpleCollectionRef + Slab<Node<AnyRange<K>, V>>> Serialize
	for generic::RangeMap<K, V, C>
{
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		let mut map = serializer.serialize_map(Some(self.range_count()))?;

		for (range, value) in self {
			map.serialize_entry(range, value)?;
		}

		map.end()
	}
}

impl<
		'de,
		K: PartialEnum + Measure + Deserialize<'de>,
		V: Deserialize<'de>,
		C: Default + SimpleCollectionRef + SimpleCollectionMut + SlabMut<Node<AnyRange<K>, V>>,
	> Deserialize<'de> for generic::RangeMap<K, V, C>
{
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		struct Visitor<K, V, C>(PhantomData<(K, V, C)>);

		impl<
				'de,
				K: PartialEnum + Measure + Deserialize<'de>,
				V: Deserialize<'de>,
				C: Default + SimpleCollectionRef + SimpleCollectionMut + SlabMut<Node<AnyRange<K>, V>>,
			> serde::de::Visitor<'de> for Visitor<K, V, C>
		{
			type Value = generic::RangeMap<K, V, C>;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				write!(formatter, "a range map")
			}

			fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::MapAccess<'de>,
			{
				let mut result = generic::RangeMap::new();

				while let Some((range, value)) = map.next_entry::<AnyRange<K>, V>()? {
					if result.insert_disconnected(range, value).is_err() {
						return Err(A::Error::custom("unexpected connected range"));
					}
				}

				Ok(result)
			}
		}

		deserializer.deserialize_map(Visitor(PhantomData))
	}
}
