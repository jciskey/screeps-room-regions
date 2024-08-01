
use std::fmt;

use std::collections::{HashMap, HashSet};
use screeps::{RoomXY, LocalCostMatrix, LocalRoomTerrain, Terrain};
use screeps::linear_index_to_xy;

use std::iter;
use itertools::Itertools;

use crate::tile_map::TileMap;
use super::super::distance_transform::DistanceTransform;
use super::watershed::{Color, RoomRegionWatershed, SegmentBorders, ColorIdx};

use screeps::constants::extra::ROOM_SIZE;
const ROOM_AREA: usize = (ROOM_SIZE as usize) * (ROOM_SIZE as usize);

#[cfg(feature = "petgraph")]
use petgraph::prelude::UnGraphMap;

/// Top-level region analysis data for a standard Screeps room
#[derive(Debug)]
pub struct RoomRegionAnalysis {
    regions: HashMap<RegionLabel, RoomRegion>,
    heights: TileMap<u8>,
    xy_label_lookup: TileMap<RegionLabel>,
    borders: SegmentBorders,
    border_tiles: Vec<RoomXY>,
    border_tile_adjacent_regions: HashMap<RoomXY, HashSet<RegionLabel>>,
    border_tiles_for_region: HashMap<RegionLabel, HashSet<RoomXY>>,
    regions_by_color_id: HashMap<ColorIdx, RegionLabel>,
    region_neighbors: HashMap<RegionLabel, HashSet<RegionLabel>>,
    exit_regions: Vec<RegionLabel>,
    watershed: RoomRegionWatershed,
    next_region_label_value: u8,
    next_border_region_label_value: u8,
}

impl RoomRegionAnalysis {
    fn new(borders: SegmentBorders, border_tiles: &[RoomXY], watershed: RoomRegionWatershed) -> Self {
        Self {
            regions: HashMap::new(),
            heights: TileMap::new(0),
            xy_label_lookup: TileMap::new(RegionLabel::new(0)),
            borders,
            border_tiles: border_tiles.to_vec(),
            border_tile_adjacent_regions: HashMap::new(),
            border_tiles_for_region: HashMap::new(),
            regions_by_color_id: HashMap::new(),
            region_neighbors: HashMap::new(),
            exit_regions: Vec::new(),
            watershed,
            next_region_label_value: 1,
            next_border_region_label_value: 1,
        }
    }

    /// Analyzes a room for region data based on terrain data
    pub fn new_from_terrain(terrain: &LocalRoomTerrain) -> Self {
        let heights = DistanceTransform::new_chebyshev(terrain);
        let watershed = RoomRegionWatershed::new_from_distance_transform(heights);
        let color_map = watershed.get_color_map().clone();
        let borders = watershed.get_borders().clone();
        let mut rra_obj = RoomRegionAnalysis::new(SegmentBorders::new(&color_map, &borders), &borders, watershed);

        rra_obj.update_height(rra_obj.watershed.get_heightmap_clone());

        // Get all colors, filter the borders and walls, aggregate members by region
        let mut members_by_maxima: HashMap<RoomXY, HashSet<RoomXY>> = HashMap::new();

        let mut maxima_by_color_id: HashMap<ColorIdx, RoomXY> = HashMap::new();

        let all_maximas = rra_obj.watershed.get_local_maximas().iter().chain(rra_obj.watershed.get_exit_region_maximas());
        for xy in all_maximas {
            let color = color_map[*xy];
            if let Color::Resolved(color_id) = color {
                maxima_by_color_id.insert(color_id, *xy);
                members_by_maxima.insert(*xy, HashSet::new());
            }
        }

        for (xy, idx) in all_room_xy_and_idx() {
            // Skip walls
            if rra_obj.get_height_for_xy(&xy) == 0 {
                continue;
            }

            match color_map[xy] {
                Color::Empty => (), // Shouldn't exist, but ignore if we do encounter it
                Color::Border => (), // Ignore borders
                Color::Pending(color_id) => (), // Shouldn't exist, ignore for now
                Color::Resolved(color_id) => {
                    if let Some(maxima) = maxima_by_color_id.get(&color_id) {
                        members_by_maxima.entry(*maxima)
                            .or_insert_with(|| HashSet::new())
                            .insert(xy);
                    }
                },
            };
        }

        let mut region_labels_by_maxima: HashMap<RoomXY, RegionLabel> = HashMap::new();
        for (maxima, members) in members_by_maxima {
            if let Color::Resolved(color_id) = color_map[maxima] {
                let label = rra_obj.new_region(&members.into_iter().collect::<Vec<_>>(), maxima, color_id);
                region_labels_by_maxima.insert(maxima, label);
            }
        }

        let exit_regions: Vec<RegionLabel> = rra_obj.watershed.get_exit_region_maximas().iter()
            .filter_map(|xy: &RoomXY| rra_obj.get_region_for_xy(*xy))
            .map(|region| region.get_label())
            .unique()
            .collect();

        rra_obj.exit_regions = exit_regions;

        let mut regions_to_connect: HashSet<(RegionLabel, RegionLabel)> = HashSet::new();
        let mut border_region_adjacencies: Vec<(RoomXY, HashSet<RegionLabel>)> = Vec::new();

        for border_xy in rra_obj.get_border_tiles() {
            let adjacent_regions: Vec<RegionLabel> = border_xy.neighbors().iter()
                .filter(|xy| terrain.get_xy(**xy) != Terrain::Wall)
                .filter(|xy| if let Color::Resolved(color_id) = color_map[*xy] { true } else { false })
                .filter_map(|xy| rra_obj.get_region_for_xy(*xy))
                .map(|region| region.get_label())
                .unique()
                .collect();

            let mut entry: HashSet<RegionLabel> = HashSet::new();
            for label in &adjacent_regions {
                entry.insert(*label);
            }

            border_region_adjacencies.push((*border_xy, entry));

            let adjacent_region_pairs: Vec<(RegionLabel, RegionLabel)> = adjacent_regions.into_iter()
                .combinations(2)
                .map(|v| (v[0], v[1]))
                .collect();

            for (label_a, label_b) in adjacent_region_pairs {
                regions_to_connect.insert((label_a, label_b));
            }
        }

        for (a, b) in regions_to_connect {
            rra_obj.connect_regions(a, b);
        }

        for label in rra_obj.regions.keys() {
            rra_obj.border_tiles_for_region.insert(*label, HashSet::new());
        }

        for (xy, entry) in border_region_adjacencies {
            for label in entry.iter() {
                rra_obj.border_tiles_for_region.entry(*label).or_insert_with(|| HashSet::new()).insert(xy);
            }
            rra_obj.border_tile_adjacent_regions.insert(xy, entry);
        }

        rra_obj
    }

    fn new_region(&mut self, members: &[RoomXY], local_maximum: RoomXY, color_id: ColorIdx) -> RegionLabel {
        let label = RegionLabel::new(self.next_region_label_value);
        self.next_region_label_value += 1;
        let region = RoomRegion::new(label, members, local_maximum, color_id);
        self.regions.insert(label, region);
        self.regions_by_color_id.insert(color_id, label);
        for xy in members {
            self.xy_label_lookup[xy] = label;
        }
        label
    }

    fn update_height(&mut self, height: TileMap<u8>) {
        self.heights = height;
    }

    fn update_height_for_xy(&mut self, xy: &RoomXY, height: u8) {
        self.heights[*xy] = height;
    }

    fn connect_regions(&mut self, a: RegionLabel, b: RegionLabel) {
        self.region_neighbors.entry(a).or_insert_with(|| HashSet::new()).insert(b);
        self.region_neighbors.entry(b).or_insert_with(|| HashSet::new()).insert(a);
    }

    /// Get all of the regions calculated for the room
    pub fn get_regions(&self) -> Vec<&RoomRegion> {
        self.regions.values().collect()
    }

    pub fn get_exit_regions(&self) -> Vec<&RoomRegion> {
        self.exit_regions.iter().map(|label| &self.regions[label]).collect()
    }

    /// Get the region for a specific `RoomXY` coordinate
    pub fn get_region_for_xy(&self, xy: RoomXY) -> Option<&RoomRegion> {
        let label = self.xy_label_lookup[xy];
        self.regions.get(&label)
    }

    /// Get the region for a specific color ID
    pub fn get_region_for_color_id(&self, color_id: ColorIdx) -> Option<&RoomRegion> {
        let label = self.regions_by_color_id.get(&color_id)?;
        self.regions.get(&label)
    }

    /// Get the height for a specific `RoomXY` coordinate
    pub fn get_height_for_xy(&self, xy: &RoomXY) -> u8 {
        self.heights[*xy]
    }

    /// Get a reference to the underlying region segment borders data
    pub fn get_border_segments(&self) -> &SegmentBorders {
        &self.borders
    }

    /// Get a reference to the underlying list of border tiles
    pub fn get_border_tiles(&self) -> &Vec<RoomXY> {
        &self.border_tiles
    }

    pub fn get_border_tiles_for_region(&self, region: RegionLabel) -> &HashSet<RoomXY> {
        &self.border_tiles_for_region[&region]
    }

    pub fn get_watershed(&self) -> &RoomRegionWatershed {
        &self.watershed
    }

    /// Generates a region adjacency graph, mapping every region (identified by its
    /// `RegionLabel`) to every adjacent region.
    pub fn get_regions_graph_as_hashmap(&self) -> &HashMap<RegionLabel, HashSet<RegionLabel>> {
        &self.region_neighbors
    }

    /// Generates an undirected graph of the regions.
    #[cfg(any(feature = "petgraph", doc))]
    #[doc(cfg(feature = "petgraph"))]
    pub fn get_regions_graph_as_graphmap(&self) -> UnGraphMap<RegionLabel, u8> {
        let num_regions = self.regions.len();
        let mut graph = UnGraphMap::with_capacity(num_regions, num_regions);

        for (label, neighbors) in &self.region_neighbors {
            for (a, b) in iter::once(label).cartesian_product(neighbors.iter()) {
                graph.add_edge(*a, *b, 1);
            }
        }

        graph
    }
}

/// Stores information about a specific region
#[derive(Debug)]
pub struct RoomRegion {
    label: RegionLabel,
    members: HashSet<RoomXY>,
    local_maximum: RoomXY,
    color_id: ColorIdx,
}

impl RoomRegion {
    fn new(label: RegionLabel, members: &[RoomXY], local_maximum: RoomXY, color_id: ColorIdx) -> Self {
        Self {
            label,
            members: HashSet::from_iter(members.iter().map(|m| *m)),
            local_maximum,
            color_id,
        }
    }

    /// Get the label that uniquely identifies this region within a specific room analysis
    pub fn get_label(&self) -> RegionLabel {
        self.label
    }

    /// Get the `RoomXY` members that comprise this region
    pub fn get_members(&self) -> Vec<RoomXY> {
        self.members.iter().map(|m| *m).collect()
    }

    /// Get the `RoomXY` tile that is the local maximum of this region
    pub fn get_local_maximum(&self) -> RoomXY {
        self.local_maximum
    }

    /// Get the color ID that uniquely identifies this region within a specific room analysis
    pub fn get_color_id(&self) -> ColorIdx {
        self.color_id
    }
}

/// Uniquely identifies a region within a specific room analysis
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Ord, PartialOrd)]
pub struct RegionLabel {
    value: u8
}

impl RegionLabel {
    fn new(value: u8) -> Self {
        Self {
            value
        }
    }
}

impl From<u8> for RegionLabel {
    fn from(value: u8) -> Self {
        Self {
            value
        }
    }
}

impl fmt::Display for RegionLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.to_string())
    }
}

#[inline]
fn all_room_xy_and_idx() -> impl Iterator<Item = (RoomXY, usize)> {
  (0..ROOM_AREA)
    .map(|idx| (linear_index_to_xy(idx), idx))
}
