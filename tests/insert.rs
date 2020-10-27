use range_map::RangeMap;

#[test]
fn insert_range() {
    let mut range_map: RangeMap<i32, bool> = RangeMap::new();

    range_map.insert_range(0..42, true);
}