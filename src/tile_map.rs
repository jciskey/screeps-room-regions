// Original under MIT license from: https://github.com/einargs/rust-screeps-code/blob/main/src/rooms/tile_slice.rs

use std::ops::{Index, IndexMut};
use screeps::{
    LocalCostMatrix, RoomXY, xy_to_linear_index,
};

use screeps::constants::extra::ROOM_SIZE;

// TODO: Remove once screeps-game-api > 0.21
const ROOM_AREA: usize = (ROOM_SIZE as usize) * (ROOM_SIZE as usize);

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

impl<T> IndexMut<usize> for TileMap<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        &mut self.0[index]
    }
}

impl<T> Index<RoomXY> for TileMap<T> {
    type Output = T;
    fn index(&self, index: RoomXY) -> &T {
        &self.0[xy_to_linear_index(index)]
    }
}

impl<T> IndexMut<RoomXY> for TileMap<T> {
    fn index_mut(&mut self, index: RoomXY) -> &mut T {
        &mut self.0[xy_to_linear_index(index)]
    }
}

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
