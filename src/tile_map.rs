// Original under MIT license from: https://github.com/einargs/rust-screeps-code/blob/main/src/rooms/tile_slice.rs

use std::ops::{Index, IndexMut};
use screeps::{
    LocalCostMatrix, RoomXY, xy_to_linear_index,
};

use screeps::constants::extra::ROOM_SIZE;

// TODO: Remove once screeps-game-api > 0.21
const ROOM_AREA: usize = (ROOM_SIZE as usize) * (ROOM_SIZE as usize);

/// Maps arbitrary data onto individual room tile positions.
#[derive(Debug, Clone)]
pub struct TileMap<T>([T; ROOM_AREA]);

impl<T> TileMap<T> where T: Copy {
    #[inline]
    pub fn new(default: T) -> TileMap<T> {
        TileMap([default; ROOM_AREA])
    }
}

impl<T: Default + Copy> Default for TileMap<T> {
    fn default() -> TileMap<T> {
        TileMap([T::default(); ROOM_AREA])
    }
}

impl<T> Index<usize> for TileMap<T> {
    type Output = T;
    fn index(&self, index: usize) -> &T {
        &self.0[index]
    }
}

/// Allows indexing by raw linear index
impl<T> IndexMut<usize> for TileMap<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        &mut self.0[index]
    }
}

/// Allows indexing by RoomXY directly
impl<T> Index<RoomXY> for TileMap<T> {
    type Output = T;
    fn index(&self, index: RoomXY) -> &T {
        &self.0[xy_to_linear_index(index)]
    }
}

/// Allows indexing by RoomXY to get a mutable copy of the associated data
impl<T> IndexMut<RoomXY> for TileMap<T> {
    fn index_mut(&mut self, index: RoomXY) -> &mut T {
        &mut self.0[xy_to_linear_index(index)]
    }
}

/// Allows indexing by RoomXY references
impl<T> Index<&RoomXY> for TileMap<T> {
    type Output = T;
    fn index(&self, index: &RoomXY) -> &T {
        &self.0[xy_to_linear_index(*index)]
    }
}

/// Allows indexing by RoomXY references to get a mutable copy of the associated data
impl<T> IndexMut<&RoomXY> for TileMap<T> {
    fn index_mut(&mut self, index: &RoomXY) -> &mut T {
        &mut self.0[xy_to_linear_index(*index)]
    }
}

/// Converts u8 data into a `LocalCostMatrix`, for convenience with
/// community methods that will often use LocalCostMatrix for the
/// same general data-association purposes `TileMap` gets used for
impl From<&TileMap<u8>> for LocalCostMatrix {
    fn from(value: &TileMap<u8>) -> Self {
        let mut lcm = LocalCostMatrix::new();

        for (xy, val) in lcm.iter_mut() {
            *val = value[xy];
        }

        lcm
    }
}

impl From<TileMap<u8>> for LocalCostMatrix {
    fn from(value: TileMap<u8>) -> Self {
        LocalCostMatrix::from(&value)
    }
}
