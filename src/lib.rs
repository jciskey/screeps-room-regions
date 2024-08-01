//! Room region analysis structures and algorithms for Screeps: World.
//!
//! # Cargo Features
//!
//! ## `petgraph`
//!
//! Enables conversion of region analysis data into graph structures that can
//! be operated on using [petgraph](https://docs.rs/petgraph/)-defined algorithms.

// to build locally with doc_cfg enabled, run:
// `RUSTDOCFLAGS="--cfg docsrs" cargo +nightly doc --all-features`
#![cfg_attr(docsrs, feature(doc_auto_cfg, doc_cfg))]
#![feature(doc_cfg)]

/// Used for calculating the standard distance transforms of tiles from walls
pub mod distance_transform;
mod disjoint_tile_set;

/// Used for top-level region analysis of rooms
pub mod region_analysis;

/// Home of the ever-helpful TileMap struct
pub mod tile_map;

// pub use crate::region_analysis::*;

use screeps::{LocalRoomTerrain};

use crate::region_analysis::structs::RoomRegionAnalysis;

/// Generates a room region analysis given the room terrain.
/// You should be able to use the returned `RoomRegionAnalysis` to get your regions for processing.
pub fn get_region_analysis_for_room_by_terrain(terrain: &LocalRoomTerrain) -> RoomRegionAnalysis {
    RoomRegionAnalysis::new_from_terrain(terrain)
}
