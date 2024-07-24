
use std::collections::{HashMap, HashSet};
use screeps::{RoomXY, LocalCostMatrix, LocalRoomTerrain};
use screeps::linear_index_to_xy;

use crate::tile_map::TileMap;
use super::super::distance_transform::DistanceTransform;
use super::watershed::{Color, RoomRegionWatershed, SegmentBorders};

use screeps::constants::extra::ROOM_SIZE;
const ROOM_AREA: usize = (ROOM_SIZE as usize) * (ROOM_SIZE as usize);

#[derive(Debug)]
pub struct RoomRegionAnalysis {
    regions: HashMap<RegionLabel, RoomRegion>,
    heights: TileMap<u8>,
    borders: SegmentBorders,
    regions_by_color_id: HashMap<u8, RegionLabel>,

    next_region_label_value: u8,
    next_border_region_label_value: u8,
}

impl RoomRegionAnalysis {
    fn new(borders: SegmentBorders) -> Self {
        Self {
            regions: HashMap::new(),
            heights: TileMap::new(0),
            borders,
            regions_by_color_id: HashMap::new(),
            next_region_label_value: 1,
            next_border_region_label_value: 1,
        }
    }

    pub fn new_from_terrain(terrain: &LocalRoomTerrain) -> Self {
        let heights = DistanceTransform::new_chebyshev(terrain);
        let watershed = RoomRegionWatershed::new_from_distance_transform(heights);
        let color_map = watershed.get_color_map();
        let borders = watershed.get_borders();
        let mut rra_obj = RoomRegionAnalysis::new(SegmentBorders::new(&color_map, &borders));

        rra_obj.update_height(watershed.get_heightmap_clone());

        // Get all colors, filter the borders and walls, aggregate members by region
        let mut members_by_maxima: HashMap<RoomXY, HashSet<RoomXY>> = HashMap::new();

        let mut maxima_by_color_id: HashMap<u8, RoomXY> = HashMap::new();

        for xy in watershed.get_local_maximas() {
            let color = color_map[*xy];
            if let Color::Resolved(color_id) = color {
                maxima_by_color_id.insert(color_id, *xy);
                members_by_maxima.insert(*xy, HashSet::new());
            }
        }

        for (xy, idx) in all_room_xy_and_idx() {
            // Skip walls
            if rra_obj.get_height_for_xy(&xy) == u8::MAX {
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

        rra_obj
    }

    pub fn new_region(&mut self, members: &[RoomXY], local_maximum: RoomXY, color_id: u8) -> RegionLabel {
        let label = RegionLabel::new(self.next_region_label_value);
        self.next_region_label_value += 1;
        let region = RoomRegion::new(label, members, local_maximum, color_id);
        self.regions.insert(label, region);
        self.regions_by_color_id.insert(color_id, label);
        label
    }

    pub fn update_height(&mut self, height: TileMap<u8>) {
        self.heights = height;
    }

    pub fn update_height_for_xy(&mut self, xy: &RoomXY, height: u8) {
        self.heights[*xy] = height;
    }

    pub fn get_regions(&self) -> Vec<&RoomRegion> {
        self.regions.values().collect()
    }

    pub fn get_region_for_xy(&self, xy: RoomXY) -> Option<&RoomRegion> {
        let label_val = self.heights[xy];
        self.regions.get(&RegionLabel::new(label_val))
    }

    pub fn get_region_for_color_id(&self, color_id: u8) -> Option<&RoomRegion> {
        let label = self.regions_by_color_id.get(&color_id)?;
        self.regions.get(&label)
    }

    pub fn get_height_for_xy(&self, xy: &RoomXY) -> u8 {
        self.heights[*xy]
    }

    pub fn get_border_segments(&self) -> &SegmentBorders {
        &self.borders
    }

    pub fn get_regions_graph(&self) -> HashMap<RegionLabel, HashSet<RegionLabel>> {
        let mut graph: HashMap<RegionLabel, HashSet<RegionLabel>> = HashMap::new();

        for label in self.regions.keys() {
            graph.insert(*label, HashSet::new());
        }

        for (color_a, color_b, _) in self.borders.iter() {
            if let Some(label_a) = self.regions_by_color_id.get(&color_a) {
                if let Some(label_b) = self.regions_by_color_id.get(&color_b) {
                    if let Some(mut edges_a) = graph.get_mut(label_a) {
                        edges_a.insert(*label_b);
                    }
                    if let Some(mut edges_b) = graph.get_mut(label_b) {
                        edges_b.insert(*label_a);
                    }
                }
            }
        }

        graph
    }
}


#[derive(Debug)]
pub struct RoomRegion {
    label: RegionLabel,
    members: HashSet<RoomXY>,
    local_maximum: RoomXY,
    color_id: u8,
}

impl RoomRegion {
    fn new(label: RegionLabel, members: &[RoomXY], local_maximum: RoomXY, color_id: u8) -> Self {
        Self {
            label,
            members: HashSet::from_iter(members.iter().map(|m| *m)),
            local_maximum,
            color_id,
        }
    }

    pub fn get_label(&self) -> RegionLabel {
        self.label
    }

    pub fn get_members(&self) -> Vec<RoomXY> {
        self.members.iter().map(|m| *m).collect()
    }

    pub fn get_local_maximum(&self) -> RoomXY {
        self.local_maximum
    }

    pub fn get_color_id(&self) -> u8 {
        self.color_id
    }
}


#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
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

#[inline]
fn all_room_xy_and_idx() -> impl Iterator<Item = (RoomXY, usize)> {
  (0..ROOM_AREA)
    .map(|idx| (linear_index_to_xy(idx), idx))
}
