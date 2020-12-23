use range_map::RangeMap;

#[test]
fn insert_single() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();
	range_map.insert(00..05, true);
	assert_eq!(range_map.range_count(), 1);
	assert_eq!(range_map.get(02), Some(&true));
}

/// Testing case (G) only.
#[test]
fn insert_exclusive_ranges() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(00..05, true);
	range_map.insert(10..15, false);
	range_map.insert(20..25, true);
	range_map.insert(30..35, false);
	range_map.insert(40..45, true);
	range_map.insert(50..55, false);
	range_map.insert(60..65, true);
	range_map.insert(70..75, false);
	range_map.insert(80..85, true);
	range_map.insert(90..95, false);

	assert_eq!(range_map.range_count(), 10);
	assert_eq!(range_map.get(02), Some(&true));
	assert_eq!(range_map.get(07), None);
	assert_eq!(range_map.get(12), Some(&false));
	assert_eq!(range_map.get(17), None);
	assert_eq!(range_map.get(22), Some(&true));
	assert_eq!(range_map.get(27), None);
	assert_eq!(range_map.get(32), Some(&false));
	assert_eq!(range_map.get(37), None);
	assert_eq!(range_map.get(42), Some(&true));
	assert_eq!(range_map.get(47), None);
	assert_eq!(range_map.get(52), Some(&false));
	assert_eq!(range_map.get(57), None);
	assert_eq!(range_map.get(62), Some(&true));
	assert_eq!(range_map.get(67), None);
	assert_eq!(range_map.get(72), Some(&false));
	assert_eq!(range_map.get(77), None);
	assert_eq!(range_map.get(82), Some(&true));
	assert_eq!(range_map.get(87), None);
	assert_eq!(range_map.get(92), Some(&false));
	assert_eq!(range_map.get(97), None);
}

#[test]
fn insert_inclusive_ranges_a() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(00..20, true); // case (G)
	range_map.insert(05..15, false); // case (A)

	assert_eq!(range_map.range_count(), 3);
	assert_eq!(range_map.get(00), Some(&true));
	assert_eq!(range_map.get(04), Some(&true));
	assert_eq!(range_map.get(05), Some(&false));
	assert_eq!(range_map.get(14), Some(&false));
	assert_eq!(range_map.get(19), Some(&true));
	assert_eq!(range_map.get(20), None);
}

#[test]
fn insert_inclusive_ranges_b() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(00..20, true); // case (G)
	range_map.insert(10..30, false); // case (B)

	assert_eq!(range_map.range_count(), 2);
	assert_eq!(range_map.get(00), Some(&true));
	assert_eq!(range_map.get(09), Some(&true));
	assert_eq!(range_map.get(10), Some(&false));
	assert_eq!(range_map.get(19), Some(&false));
	assert_eq!(range_map.get(29), Some(&false));
	assert_eq!(range_map.get(30), None);
}

#[test]
fn insert_inclusive_ranges_ce() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(10..30, true); // case (G)
	range_map.insert(00..20, false); // cases (C) and (E)

	assert_eq!(range_map.range_count(), 2);
	assert_eq!(range_map.get(00), Some(&false));
	assert_eq!(range_map.get(09), Some(&false));
	assert_eq!(range_map.get(10), Some(&false));
	assert_eq!(range_map.get(19), Some(&false));
	assert_eq!(range_map.get(20), Some(&true));
	assert_eq!(range_map.get(29), Some(&true));
	assert_eq!(range_map.get(30), None);
}

#[test]
fn insert_inclusive_ranges_bcd() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(10..15, true); // case (G)
	range_map.insert(20..25, true); // case (G)
	range_map.insert(30..35, true); // case (G)
	range_map.insert(10..35, false); // case (B, C, D)

	assert_eq!(range_map.range_count(), 1);
	assert_eq!(range_map.get(10), Some(&false));
	assert_eq!(range_map.get(14), Some(&false));
	assert_eq!(range_map.get(17), Some(&false));
	assert_eq!(range_map.get(20), Some(&false));
	assert_eq!(range_map.get(24), Some(&false));
	assert_eq!(range_map.get(27), Some(&false));
	assert_eq!(range_map.get(30), Some(&false));
	assert_eq!(range_map.get(34), Some(&false));
	assert_eq!(range_map.get(35), None);
}

#[test]
fn insert_inclusive_ranges_cf() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(0..10, true); // case (G)
	range_map.insert(20..30, true); // case (G)
	range_map.insert(15..25, false); // case (C, F)

	assert_eq!(range_map.range_count(), 3);
	assert_eq!(range_map.get(0), Some(&true));
	assert_eq!(range_map.get(9), Some(&true));
	assert_eq!(range_map.get(10), None);
	assert_eq!(range_map.get(15), Some(&false));
	assert_eq!(range_map.get(24), Some(&false));
	assert_eq!(range_map.get(25), Some(&true));
	assert_eq!(range_map.get(29), Some(&true));
	assert_eq!(range_map.get(30), None);
}

#[test]
fn insert_merge_ranges() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(00..10, true);
	range_map.insert(10..20, true);

	assert_eq!(range_map.range_count(), 1);
	assert_eq!(range_map.get(0), Some(&true));
	assert_eq!(range_map.get(19), Some(&true));
	assert_eq!(range_map.get(20), None);
}

#[test]
fn insert_merge_ranges_rev() {
	let mut range_map: RangeMap<i32, bool> = RangeMap::new();

	range_map.insert(10..20, true);
	range_map.insert(00..10, true);

	assert_eq!(range_map.range_count(), 1);
	assert_eq!(range_map.get(0), Some(&true));
	assert_eq!(range_map.get(19), Some(&true));
	assert_eq!(range_map.get(20), None);
}