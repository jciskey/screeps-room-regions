# Changelog

## Unreleased

- Remove `priority_queue` dependency in favor of `std::collections::BinaryTree`
- Bugfix for region lookup by `RoomXY`
- Add border tile region adjacency information
- Add region border tile adjacency information

## 0.1.3

- Add distinct exit regions to watershed process
- Expose direct border tiles in `RoomRegionAnalysis` via `get_border_tiles` function

## 0.1.2

- Improve documentation

## 0.1.1

- Add `get_region_analysis_for_room_by_terrain` function
- Add `RoomRegionAnalysis` struct

## 0.1.0

- Initial empty release
