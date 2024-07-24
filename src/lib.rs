
pub mod distance_transform; // For testing purposes
mod disjoint_tile_set;
mod region_analysis;
mod tile_map;

use screeps::{LocalRoomTerrain};

pub use crate::region_analysis::RoomRegionAnalysis;

pub fn get_region_analysis_for_room_by_terrain(terrain: &LocalRoomTerrain) -> RoomRegionAnalysis {
    RoomRegionAnalysis::new_from_terrain(terrain)
}
