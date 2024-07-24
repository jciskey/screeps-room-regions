// Original under MIT license from: https://github.com/einargs/rust-screeps-code/blob/main/src/rooms/room_cut.rs

use std::ops::Range;
use std::fmt;
use std::collections::BTreeMap;
use std::collections::HashSet;

use super::super::tile_map::TileMap;
use super::super::distance_transform::DistanceTransform;
use super::super::disjoint_tile_set::DisjointTileSet;

use screeps::linear_index_to_xy;
use screeps::{Direction, RoomXY};

use screeps::constants::extra::ROOM_SIZE;
const ROOM_AREA: usize = (ROOM_SIZE as usize) * (ROOM_SIZE as usize);

/// Stores data from a maxima-based Meyer's Floodfill Algorithm.
pub struct RoomRegionWatershed {
    heights: DistanceTransform,
    local_maximas: Vec<RoomXY>,
    color_map: TileMap<Color>,
    num_colors: ColorIdx,
    borders: Vec<RoomXY>,
}

impl RoomRegionWatershed {
    fn new(heights: DistanceTransform, local_maximas: &[RoomXY], color_map: TileMap<Color>, num_colors: ColorIdx, borders: &[RoomXY]) -> Self {
        Self {
            heights,
            local_maximas: local_maximas.to_vec(),
            color_map,
            num_colors,
            borders: borders.to_vec(),
        }
    }

    /// Processes baseline distance transform data into a `RoomRegionWatershed`.
    pub fn new_from_distance_transform(heights: DistanceTransform) -> Self {
        // Find local maximas from the DT using union-find
        let (maxima_iter, _) = calculate_local_maxima(&heights);
        let maxima_set: HashSet<RoomXY> = maxima_iter.collect();
        let (color_map, color_count, borders) = flood_color_map(
            &heights,
            &maxima_set.iter().map(|xy| *xy).collect::<Vec<RoomXY>>(),
        );

        Self {
            heights,
            local_maximas: maxima_set.into_iter().collect(),
            color_map,
            num_colors: color_count,
            borders,
        }
    }

    /// Gets a full clone of the heightmap
    pub fn get_heightmap_clone(&self) -> TileMap<u8> {
        self.heights.get_values().clone()
    }

    /// Gets a reference to the tile color mapping
    pub fn get_color_map(&self) -> &TileMap<Color> {
        &self.color_map
    }

    /// Gets a list of the local maximas across the entire room
    pub fn get_local_maximas(&self) -> &Vec<RoomXY> {
        &self.local_maximas
    }

    /// Gets a list of the border tiles in the room (tiles that
    /// are adjacent to more than 1 region, and thus are
    /// unassigned to any region)
    pub fn get_borders(&self) -> &Vec<RoomXY> {
        &self.borders
    }
}

/// Color is used to identify which room a source belongs to when performing
/// bfs.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Color {
    Empty,
    Border,
    Pending(ColorIdx),
    Resolved(ColorIdx)
}

pub type ColorIdx = u8;

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Color::Empty => write!(f, "E"),
            Color::Border => write!(f, "B"),
            Color::Pending(idx) => write!(f, "P{idx}"),
            Color::Resolved(idx) => write!(f, "{idx}"),
        }
    }
}


/// Flood a color map, avoiding coloring a cell when it would cause
/// diagonals or adjacents of one color to touch another color.
/// Returns a list of coordinates that are on the border between
/// two colors.
///
/// Uses BFS with a priority queue, which maintains fairness.
fn flood_color_map(
    height_map: &DistanceTransform,
    local_maximas: &[RoomXY],
) -> (TileMap<Color>, ColorIdx, Vec<RoomXY>) {
    use priority_queue::PriorityQueue;
    let mut queue: PriorityQueue<RoomXY, u8> = PriorityQueue::new();
    let mut borders: Vec<RoomXY> = Vec::new();
    let mut color_map: TileMap<Color> = TileMap::new(Color::Empty);

    let mut color_count: ColorIdx = 0;
    for maxima in local_maximas {
        color_map[*maxima] = Color::Resolved(color_count);
        color_count += 1;
        let height = height_map.get(*maxima);
        queue.push(*maxima, height);
    }

    while let Some((xy, height)) = queue.pop() {
        let xy_color = color_map[xy];
        let xy_height = height_map.get(xy);
        // The maxima are the only points that should have a color when
        // this stage happens.
        let xy_color_idx = match xy_color {
            Color::Pending(color) => {
                // We check to see if there's a resolved neighbor that has a different color.
                // If there is, we become a border and continue to the next tile.
                let neighboring_other_color = xy.neighbors().iter().any(|adj| match color_map[*adj] {
                    Color::Resolved(other_color) => other_color != color,
                    // if it's a pending, we'll end up on that pending looking at this
                    // tile later.
                    _ => false,
                });
                if neighboring_other_color {
                    color_map[xy] = Color::Border;
                    borders.push(xy);
                    continue
                } else {
                    color_map[xy] = Color::Resolved(color);
                }
                color
            }
            // only the maxima should have a resolved color right now
            Color::Resolved(color) => color,
            Color::Border | Color::Empty => {
                panic!("color at {xy} was {xy_color} when it should be pending or resolved (if it's a maxima)");
            }
        };

        for adj in xy.neighbors() {
            let adj_height = height_map.get(adj);
            // TODO: maybe skip if higher than xy_height. If it's higher, someone else
            // will reach it? that's just a minor optimization I think? If it's
            // taxicab, then definitely there's no way for a tile to see a higher
            // piece that isn't in the queue, but with chessboard I think there is.
            // Imagine: 3 2 3, where xy is the left 3. We get to the 2 as adj, which
            // can then see the right 3.

            // we skip walls
            if adj_height == 0 {
                continue
            }

            match color_map[adj] {
                Color::Empty => {
                    let adj_height = height_map.get(adj);

                    color_map[adj] = Color::Pending(xy_color_idx);
                    queue.push(adj, adj_height);
                }
                _ => {
                    continue
                }
            }
        }
    }

    (color_map, color_count, borders)
}

/// Get a list of maximas.
///
/// Creates a list of local maxima and a [`DisjointTileSet`]
/// of how it was found. This is not good segmentation, but
/// is useful for debugging.
fn calculate_local_maxima(
    height_map: &DistanceTransform
) -> (impl Iterator<Item = RoomXY>, DisjointTileSet) {
    let mut dts = DisjointTileSet::new(height_map);

    for (xy, idx) in all_room_xy_and_idx() {
        // skip if it's a wall
        if height_map.get_index(idx as usize) == 0 {
            continue
        }
        // skip if we've *definitely* visited already
        if !dts.is_singleton(idx as u16) {
            continue
        }
        link_to_maxima(xy, height_map, &mut dts);
    }

    let mut maxima_map: BTreeMap<u16, RoomXY> = BTreeMap::new();

    // color the map and create a list of indexes.
    for idx in 0..(ROOM_AREA as u16) {
        if height_map.get_index(idx as usize) == 0 {
            continue
        }
        let maxima = dts.maxima_for(idx);
        maxima_map.entry(idx).or_insert(maxima);
    }

    (maxima_map.into_values(), dts)
}

/// Visit a tile to add it to the DisjointTileSet regions.
///
/// Returns true if we found a higher tile.
fn link_to_maxima(
    xy: RoomXY,
    height_map: &DistanceTransform,
    dts: &mut DisjointTileSet,
) -> bool {
    let height = height_map.get(xy);

    if let Some(higher) = xy.neighbors()
        .iter()
        .find(|adj| height_map.get(**adj) > height)
    {
        let was_single = dts.is_singleton_xy(*higher);
        dts.union_xy(*higher, xy);
        if was_single {
            link_to_maxima(*higher, height_map, dts);
        }
        return true;
    }

    let equals = xy.neighbors()
        .into_iter()
        .filter(|adj| height_map.get(*adj) == height);

    for equal in equals {
        if !dts.is_singleton_xy(equal) {
            let higher = dts.maxima_height_for_xy(equal) > height;
            dts.union_xy(equal, xy);
            if higher {
                return true
            }
        } else {
            dts.union_xy(equal, xy);
            if link_to_maxima(equal, height_map, dts) {
                return true
            }
        }
    }
    return false
}

#[inline]
fn all_room_xy_and_idx() -> impl Iterator<Item = (RoomXY, usize)> {
  (0..ROOM_AREA)
    .map(|idx| (linear_index_to_xy(idx), idx))
}

/// Contains information about an individual border between two regions.
#[derive(Clone, Debug, Default)]
pub struct SegmentBorder {
  walls: Vec<RoomXY>
}

impl SegmentBorder {
  #[inline]
  fn add_wall(&mut self, xy: RoomXY) {
    self.walls.push(xy);
  }

  /// The number of tiles that comprise this border segment
  #[inline]
  pub fn len(&self) -> usize {
    self.walls.len()
  }

  /// Generates an iterator over the `RoomXY` tiles that comprise this border segment
  #[inline]
  pub fn walls(&self) -> impl Iterator<Item = RoomXY> + '_ {
    self.walls.iter().map(|x| *x)
  }
}

/// Data structure representing information about the borders
/// between regions.
///
/// `SegmentBorder` indexes are unique because we order them before inserts into the underlying `BTreeMap`.
#[derive(Debug)]
pub struct SegmentBorders(BTreeMap<(ColorIdx, ColorIdx), SegmentBorder>);

/// Quick utility function.
fn unique_pairs(range: Range<usize>) -> impl Iterator<Item = (usize, usize)> {
  use std::iter::repeat;
  range.clone().flat_map(move |i| repeat(i).zip((i+1)..range.end))
}

impl SegmentBorders {
  pub fn new(
    color_map: &TileMap<Color>,
    border_xys: &[RoomXY]
  ) -> SegmentBorders {
    use Color::*;
    use itertools::Itertools;

    #[inline]
    fn adj_color_pairs(
      color_map: &TileMap<Color>,
      neighbors: impl Iterator<Item = RoomXY>
    ) -> impl Iterator<Item = (ColorIdx, ColorIdx)> {
      let adj_colors: Vec<ColorIdx> = neighbors
        .filter_map(|adj| match color_map[adj] {
          Resolved(color) => Some(color),
          _ => None,
        })
      // it would probably be faster to insert and use .contains
      // since this uses a hash map, but not worth it.
        .unique()
        .collect();
      unique_pairs(0..adj_colors.len())
        .map(move |(i,j)| (adj_colors[i], adj_colors[j]))
    }

    let mut seg_borders = SegmentBorders(BTreeMap::new());
    for border_xy in border_xys {
      // we do two rounds: an initial round using taxicab and creates
      // borders if they don't exist already, and then a second that
      // only adds to existing borders.

      // we use taxicab adjacent so that in this situation:
      // 1 1 1 E 2
      // B B B E 2
      // 3 3 B 2 2
      // we don't end up with 1 connected to 2.
      // However, we still want  the corner piece to be included in
      // 1-3 and 3-2. This is done in the second phase.
      for (a,b) in adj_color_pairs(color_map, taxicab_adjacent(*border_xy)) {
        seg_borders.get_mut_or_default(a, b);
      }

      for (a,b) in adj_color_pairs(color_map, border_xy.neighbors().into_iter()) {
        if let Some(border) = seg_borders.get_mut(a, b) {
          border.add_wall(*border_xy);
        }
      }
    }

    seg_borders
  }

  fn get_mut_or_default(&mut self, a: ColorIdx, b: ColorIdx) -> &mut SegmentBorder {
    let idx = if a < b { (a,b) } else { (b,a) };
    self.0.entry(idx).or_default()
  }

  fn get_mut(&mut self, a: ColorIdx, b: ColorIdx) -> Option<&mut SegmentBorder> {
    let idx = if a < b { (a,b) } else { (b,a) };
    self.0.get_mut(&idx)
  }

  /// Iterates over border segments, along with the color IDs for the regions that the border separates
  #[inline]
  pub fn iter(&self) -> impl Iterator<Item = (ColorIdx, ColorIdx, &SegmentBorder)> {
    self.0.iter().map(|((a,b), border)| (*a, *b, border))
  }
}

const TAXICAB_DIRECTIONS: [Direction; 4] = [
  Direction::Top,
  Direction::Bottom,
  Direction::Left,
  Direction::Right,
];

#[inline]
fn taxicab_adjacent(xy: RoomXY) -> impl Iterator<Item = RoomXY> {
  use Direction::*;
  TAXICAB_DIRECTIONS.into_iter()
    .filter_map(move |dir| xy.checked_add_direction(dir))
}
