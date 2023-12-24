use crate::geodata::importer::{Indexed, IndexedNode, IndexedPolygon, IndexedRelation, IndexedWay, ParsedNode};
#[cfg(test)]
use crate::geodata::importer::{OsmRef, Parsed};
use crate::tile::tile;
use anyhow::{bail, Result};
use byteorder::{LittleEndian, WriteBytesExt};
use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::Write;

pub(super) type RawRefs = Vec<usize>;

#[derive(Default)]
struct TileReferences {
    local_node_ids: BTreeSet<usize>,
    local_way_ids: BTreeSet<usize>,
    local_multipolygon_ids: BTreeSet<usize>,
}

#[derive(Default)]
struct TileIdToReferences {
    refs: BTreeMap<(u32, u32), TileReferences>,
}

pub(super) fn save_to_internal_format(writer: &mut dyn Write, indexed: &Indexed) -> Result<()> {
    let mut buffered_data = BufferedData::default();
    save_nodes(writer, &indexed.nodes, &mut buffered_data)?;
    save_ways(writer, &indexed.ways, &mut buffered_data)?;
    save_polygons(writer, &indexed.polygons, &mut buffered_data)?;
    save_relations(writer, &indexed.relations, &mut buffered_data)?;

    let tile_references = get_tile_references(indexed);
    save_tile_references(writer, &tile_references, &mut buffered_data)?;

    buffered_data.save(writer)?;

    Ok(())
}

impl TileIdToReferences {
    fn tile_ref_by_node(&mut self, node: &ParsedNode) -> &mut TileReferences {
        let node_tile = tile::coords_to_max_zoom_tile(node);
        self.tile_ref_by_xy(node_tile.x, node_tile.y)
    }

    fn tile_ref_by_xy(&mut self, tile_x: u32, tile_y: u32) -> &mut TileReferences {
        self.refs.entry((tile_x, tile_y)).or_insert_with(Default::default)
    }
}

fn save_nodes(writer: &mut dyn Write, indexes: &[IndexedNode], data: &mut BufferedData) -> Result<()> {
    let mut tags_counter: usize = 0;
    let mut nodes_counter: usize = 0;

    writer.write_u32::<LittleEndian>(to_u32_safe(indexes.len())?)?;
    for node_ref in indexes {
        writer.write_u64::<LittleEndian>(node_ref.node.id)?;
        writer.write_f64::<LittleEndian>(node_ref.node.lat)?;
        writer.write_f64::<LittleEndian>(node_ref.node.lon)?;
        nodes_counter += 1;
        save_tags(writer, &node_ref.node.tags, data)?;
        tags_counter += node_ref.node.tags.len();
    }
    println!("writed {} nodes with {} tags", nodes_counter, tags_counter);
    Ok(())
}

fn save_ways(writer: &mut dyn Write, indexes: &[IndexedWay], data: &mut BufferedData) -> Result<()> {
    let mut tags_counter = 0;
    let mut ways_counter = 0;

    writer.write_u32::<LittleEndian>(to_u32_safe(indexes.len())?)?;
    for way_ref in indexes {
        writer.write_u64::<LittleEndian>(way_ref.way.id)?;
        save_refs(writer, way_ref.nodes_ref.iter(), data)?;
        save_tags(writer, &way_ref.way.tags, data)?;
        tags_counter += way_ref.way.tags.len();
        ways_counter += 1;
    }

    println!("writed {} ways with {} tags", ways_counter, tags_counter);
    Ok(())
}

fn save_polygons(writer: &mut dyn Write, indexes: &[IndexedPolygon], data: &mut BufferedData) -> Result<()> {
    writer.write_u32::<LittleEndian>(to_u32_safe(indexes.len())?)?;
    for polygon in indexes {
        save_refs(writer, polygon.nodes_ref.iter(), data)?;
    }
    println!("writed {} polygons", indexes.len());
    Ok(())
}

fn save_relations(writer: &mut dyn Write, indexes: &[IndexedRelation], data: &mut BufferedData) -> Result<()> {
    let mut tags_counter: usize = 0;
    writer.write_u32::<LittleEndian>(to_u32_safe(indexes.len())?)?;
    for rel_ref in indexes {
        writer.write_u64::<LittleEndian>(rel_ref.relation.id)?;
        save_refs(writer, rel_ref.polygons_ref.iter(), data)?;
        save_tags(writer, &rel_ref.relation.tags, data)?;
        tags_counter += rel_ref.relation.tags.len();
    }
    println!("writed {} multipolygons with {} tags", indexes.len(), tags_counter);
    Ok(())
}

fn save_tile_references(
    writer: &mut dyn Write,
    tile_references: &TileIdToReferences,
    data: &mut BufferedData,
) -> Result<()> {
    writer.write_u32::<LittleEndian>(to_u32_safe(tile_references.refs.len())?)?;
    for (k, v) in &tile_references.refs {
        writer.write_u32::<LittleEndian>(k.0)?;
        writer.write_u32::<LittleEndian>(k.1)?;

        save_refs(writer, v.local_node_ids.iter(), data)?;
        save_refs(writer, v.local_way_ids.iter(), data)?;
        save_refs(writer, v.local_multipolygon_ids.iter(), data)?;
    }

    Ok(())
}

fn save_refs<'a, I>(writer: &mut dyn Write, refs: I, data: &mut BufferedData) -> Result<()>
where
    I: Iterator<Item = &'a usize>,
{
    let offset = data.all_ints.len();
    for r in refs {
        data.all_ints.push(to_u32_safe(*r)?);
    }
    writer.write_u32::<LittleEndian>(to_u32_safe(offset)?)?;
    writer.write_u32::<LittleEndian>(to_u32_safe(data.all_ints.len() - offset)?)?;
    Ok(())
}

fn save_tags(writer: &mut dyn Write, tags: &BTreeMap<String, String>, data: &mut BufferedData) -> Result<()> {
    let mut kv_refs = RawRefs::new();

    for (k, v) in tags.iter() {
        let (k_offset, k_length) = data.add_string(k);
        let (v_offset, v_length) = data.add_string(v);
        kv_refs.extend([k_offset, k_length, v_offset, v_length].iter());
    }

    save_refs(writer, kv_refs.iter(), data)?;

    Ok(())
}

#[derive(Default)]
struct BufferedData {
    all_ints: Vec<u32>,
    string_to_offset: HashMap<String, usize>,
    all_strings: Vec<u8>,
}

impl BufferedData {
    fn add_string(&mut self, s: &str) -> (usize, usize) {
        let bytes = s.as_bytes();
        let all_strings = &mut self.all_strings;
        let offset = self.string_to_offset.entry(s.to_string()).or_insert_with(|| {
            let offset = all_strings.len();
            all_strings.extend_from_slice(bytes);
            offset
        });
        (*offset, bytes.len())
    }

    fn save(&self, writer: &mut dyn Write) -> Result<()> {
        writer.write_u32::<LittleEndian>(to_u32_safe(self.all_ints.len())?)?;
        for i in &self.all_ints {
            writer.write_u32::<LittleEndian>(*i)?;
        }
        writer.write_all(&self.all_strings)?;
        Ok(())
    }
}

fn get_tile_references(indexed: &Indexed) -> TileIdToReferences {
    let mut result = TileIdToReferences::default();

    for (i, node) in indexed.nodes.iter().map(|node_ref| node_ref.node).enumerate() {
        result.tile_ref_by_node(node).local_node_ids.insert(i);
    }

    for (i, way) in indexed.ways.iter().enumerate() {
        let nodes = way
            .nodes_ref
            .iter()
            .map(|node_local_id| indexed.nodes[*node_local_id].node);

        insert_entity_id_to_tiles(&mut result, nodes, |x| &mut x.local_way_ids, i);
    }

    let polygons = &indexed.polygons;
    for (i, multipolygon) in indexed.relations.iter().enumerate() {
        let node_ids = multipolygon
            .polygons_ref
            .iter()
            .flat_map(move |local_poly_id| polygons[*local_poly_id].nodes_ref.iter())
            .map(|node_local_id| indexed.nodes[*node_local_id].node);
        insert_entity_id_to_tiles(&mut result, node_ids, |x| &mut x.local_multipolygon_ids, i);
    }

    result
}

fn insert_entity_id_to_tiles<'a, I>(
    result: &mut TileIdToReferences,
    mut nodes: I,
    get_refs: impl Fn(&mut TileReferences) -> &mut BTreeSet<usize>,
    entity_id: usize,
) where
    I: Iterator<Item = &'a ParsedNode>,
{
    let first_node = match nodes.next() {
        Some(n) => n,
        _ => return,
    };

    let first_tile = tile::coords_to_max_zoom_tile(first_node);
    let mut tile_range = tile::TileRange {
        min_x: first_tile.x,
        max_x: first_tile.x,
        min_y: first_tile.y,
        max_y: first_tile.y,
    };
    for node in nodes {
        let next_tile = tile::coords_to_max_zoom_tile(node);
        tile_range.min_x = min(tile_range.min_x, next_tile.x);
        tile_range.max_x = max(tile_range.max_x, next_tile.x);
        tile_range.min_y = min(tile_range.min_y, next_tile.y);
        tile_range.max_y = max(tile_range.max_y, next_tile.y);
    }
    for x in tile_range.min_x..=tile_range.max_x {
        for y in tile_range.min_y..=tile_range.max_y {
            get_refs(result.tile_ref_by_xy(x, y)).insert(entity_id);
        }
    }
}

fn to_u32_safe(num: usize) -> Result<u32> {
    if num > (u32::max_value() as usize) {
        bail!("{} doesn't fit into u32", num);
    }
    Ok(num as u32)
}

