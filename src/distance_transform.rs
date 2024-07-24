// Original under MIT license from: https://github.com/einargs/rust-screeps-code/blob/main/src/rooms/dist_transform.rs

use std::fmt;
use std::collections::{HashMap, HashSet};

use screeps::{Direction, LocalRoomTerrain, RoomXY, Terrain};
use screeps::linear_index_to_xy;

use screeps::constants::extra::ROOM_SIZE;

// TODO: Remove once screeps-game-api > 0.21
const ROOM_AREA: usize = (ROOM_SIZE as usize) * (ROOM_SIZE as usize);

use crate::tile_map::TileMap;

/// Stores distance data for all of the tiles in a standard Screeps room.
#[derive(Debug)]
pub struct DistanceTransform {
    values: TileMap<u8>,
    xy_by_distance: HashMap<u8, HashSet<RoomXY>>,
}

impl DistanceTransform {
    pub fn empty() -> Self {
        Self {
            values: TileMap::new(0),
            xy_by_distance: HashMap::new(),
        }
    }

    /// This uses chebyshev distance, or chessboard distance. Tiles are connected
    /// to their diagonals (8-connected).
    pub fn new_chebyshev(terrain: &LocalRoomTerrain) -> DistanceTransform {
        use Direction::*;
        let forward_directions = [ BottomLeft, Left, TopLeft, Top, TopRight ];
        let backward_directions = [ BottomLeft, Bottom, BottomRight, Right, TopRight ];
        DistanceTransform::mk_with_dirs(terrain, &forward_directions, &backward_directions)
    }

    /// Uses manhattan distance, or taxicab distance. Tiles are only connected
    /// vertically and horizontally (4-connected).
    pub fn new_manhattan(terrain: &LocalRoomTerrain) -> DistanceTransform {
        use Direction::*;
        let forward_directions = [ Left, Top ];
        let backward_directions = [ Bottom, Right ];
        DistanceTransform::mk_with_dirs(terrain, &forward_directions, &backward_directions)
    }

    /// Get the distance using a coordinate pair.
    pub fn get(&self, xy: RoomXY) -> u8 {
        self.values[xy]
    }

    /// Get the distance using the linear index.
    pub fn get_index(&self, idx: usize) -> u8 {
        self.values[idx]
    }

    /// Get a reference to the raw `TileMap`.
    pub fn get_values(&self) -> &TileMap<u8> {
        &self.values
    }

    /// Get a lookup `HashMap` that maps distances to `RoomXY` that have that distance value
    pub fn get_xy_by_distance_lookup(&self) -> &HashMap<u8, HashSet<RoomXY>> {
        &self.xy_by_distance
    }

    /// Update a specific tile index to have a new value
    pub fn set(&mut self, idx: usize, val: u8) {
        let xy = linear_index_to_xy(idx);
        let oldval = self.values[xy];
        self.values[xy] = val;
        if oldval != val {
            if let Some(set) = self.xy_by_distance.get_mut(&oldval) {
                set.remove(&xy);
            }
            self.xy_by_distance.entry(val).or_insert_with(|| HashSet::new()).insert(xy);
        }
    }
}

impl fmt::Display for DistanceTransform {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for y in 0..ROOM_SIZE {
      let first_xy = RoomXY::try_from((0, y))
        .expect("Should be valid coords");
      write!(f, "{:2}", self.get(first_xy))?;
      for x in 1..ROOM_SIZE {
        let xy = RoomXY::try_from((x, y))
          .expect("Should be valid coords");
        write!(f, " {:2}", self.get(xy))?;
      }
      write!(f, "\n")?;
    }
    Ok(())
  }
}

impl DistanceTransform {

    fn mk_with_dirs(
      terrain: &LocalRoomTerrain,
      forward_directions: &[Direction],
      backward_directions: &[Direction],
    ) -> DistanceTransform {
        use Direction::*;
        let mut data: TileMap<u8> = TileMap::new(0);
        let mut distances_lookup: HashMap<u8, HashSet<RoomXY>> = HashMap::new();

        // We initialize the walls to be 0 and everything else to be the
        // max possible distance.
        for idx in 0..ROOM_AREA {
            data[idx] = match terrain.get_xy(linear_index_to_xy(idx)) {
                Terrain::Wall => 0,
                _ => u8::MAX,
            };
        }

      // We do our first pass going from the top left to the bottom right

        for x in 0..ROOM_SIZE {
            for y in 0..ROOM_SIZE {
                let xy = RoomXY::try_from((x, y))
                    .expect("Should be valid coords");
                DistanceTransform::apply_directions(xy, forward_directions, &mut data);
            }
        }

        // We do our second pass going from the bottom right to the top left
        for x in (0..ROOM_SIZE).rev() {
            for y in (0..ROOM_SIZE).rev() {
                let xy = RoomXY::try_from((x, y))
                    .expect("Should be valid coords");
                DistanceTransform::apply_directions(xy, backward_directions, &mut data);

                // Since we know the value of this cell won't change after this, we can
                // store the lookup distance of this cell
                let value = data[xy];
                distances_lookup.entry(value)
                    .or_insert_with(|| HashSet::<RoomXY>::new())
                    .insert(xy);
            }
        }

        DistanceTransform {
            values: data,
            xy_by_distance: distances_lookup,
        }
    }

    /// Performs a pass update on a cell, updating it's new value with information
    /// from the cells in the nearby given directions.
    #[inline]
    fn apply_directions(
        xy: RoomXY,
        directions: &[Direction],
        data: &mut TileMap<u8>
    ) {
        let prev = data[xy];

        // Calculate the new value for this cell by looking at the minimum value of adjacent
        // cells + 1 and it's current value.
        let new_value = directions
            .iter()
            .filter_map(|dir| {
              // We want to make sure that exits have the correct height, so we
              // don't default to 0 when we end up out of bounds.
              xy.checked_add_direction(*dir)
                .map(|adjacent| (&data)[adjacent].saturating_add(1))
            })
            .chain(std::iter::once(prev))
            .min()
            .expect("Cannot be zero length because of the once");

        data[xy] = new_value;
    }
}