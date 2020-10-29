// use range_map::RangeMap;

// #[test]
// fn remove_range() {
// 	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

// 	range_map.insert_range(00..10, true);
// 	range_map.remove_range(1..9);

// 	assert_eq!(range_map.range_count(), 2);
// 	assert_eq!(range_map.get(0), Some(&true));
// 	assert_eq!(range_map.get(1), None);
// 	assert_eq!(range_map.get(8), None);
// 	assert_eq!(range_map.get(9), Some(&true));
// 	assert_eq!(range_map.get(10), None);
// }