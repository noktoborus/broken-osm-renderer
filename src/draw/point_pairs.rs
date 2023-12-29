use crate::draw::labelable::TiledEntity;
use crate::draw::point::Point;
use crate::geodata::reader::{Multipolygon, Polygon, Way};
use crate::mapcss::styler::OsmEntityType;
use crate::tile::tile::Tile;

pub type PointPairIter<'a> = Box<dyn Iterator<Item = (Point, Point)> + 'a>;

pub trait PointPairCollection<'a> {
    fn to_point_pairs(&'a self, scale: f64) -> PointPairIter<'a>;
}

macro_rules! implement_to_point_pairs {
    ($s:expr, $tile:expr, $scale:expr) => {
        Box::new((1..$s.node_count()).map(move |idx| {
            let n1 = $s.get_node(idx - 1);
            let n2 = $s.get_node(idx);
            (
                Point::from_node(&n1, $tile, $scale),
                Point::from_node(&n2, $tile, $scale),
            )
        }))
    };
}

fn way_to_point_pairs<'w>(way: &'w Way, tile: &'w Tile, scale: f64) -> PointPairIter<'w> {
    implement_to_point_pairs!(way, tile, scale)
}

fn polygon_into_point_pairs<'p>(polygon: Polygon<'p>, tile: &'p Tile, scale: f64) -> PointPairIter<'p> {
    implement_to_point_pairs!(polygon, tile, scale)
}

fn multipolygon_to_point_pairs<'r>(rel: &'r Multipolygon, tile: &'r Tile, scale: f64) -> PointPairIter<'r> {
    let polygon_count = rel.polygon_count();
    Box::new((0..polygon_count).flat_map(move |idx| polygon_into_point_pairs(rel.get_polygon(idx), tile, scale)))
}

impl<'a, 'wp> PointPairCollection<'a> for TiledEntity<'a, 'wp> {
    fn to_point_pairs(&'a self, scale: f64) -> PointPairIter<'a> {
        match self.entity {
            OsmEntityType::Node(_) => todo!(),
            OsmEntityType::Way(way) => way_to_point_pairs(way, &self.tile, scale),
            OsmEntityType::Multipolygon(mp) => multipolygon_to_point_pairs(mp, &self.tile, scale),
        }
    }
}
