use crate::coords;
use crate::geodata::collect_polygons;
use crate::geodata::saver::save_to_internal_format;
use crate::mapcss::filterer::Filterer;
use crate::mapcss::parser::ObjectType;
use anyhow::{anyhow, bail, Context, Result};
#[cfg(feature = "pbf")]
use osmpbf::{Element, ElementReader, RelMemberType};
use quick_xml::events::attributes::Attributes;
use quick_xml::events::{BytesStart, Event};
use quick_xml::reader::Reader;
use std::borrow::Cow;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashSet;
use std::collections::{BTreeMap, HashMap};
use std::ffi::OsStr;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::prelude::*;
use std::io::{BufReader, BufWriter};
use std::path::Path;

pub fn import<P: AsRef<Path>>(input: P, output: P, filterer: Option<&Filterer>) -> Result<()> {
    let output_file = File::create(output.as_ref()).context(format!(
        "Failed to open {} for writing",
        output.as_ref().to_string_lossy()
    ))?;
    let mut writer = BufWriter::new(output_file);

    let parsed = match input.as_ref().extension().and_then(OsStr::to_str) {
        Some("osm") => parse_osm_xml(input, filterer)?,
        Some("xml") => parse_osm_xml(input, filterer)?,
        //#[cfg(feature = "pbf")]
        //Some("pbf") => parse_pbf(input)?,
        _ => bail!("Extension not supported"),
    };

    println!("*** Build indexes");
    let mut index: Indexed<'_> = Indexed::new();
    index.build_from(&parsed);
    index.print_stats();

    println!("*** Converting geodata to internal format");
    save_to_internal_format(&mut writer, &index).context("Failed to write the imported data to the output file")?;
    Ok(())
}

#[cfg(feature = "pbf")]
fn parse_pbf<P: AsRef<Path>>(input: P, filterer: Option<&Filterer>) -> Result<Parsed> {
    let mut entity_storages = Parsed::new();
    let mut elem_count = 0;
    println!("Parsing PBF");

    let reader = ElementReader::from_path(input)?;
    reader.for_each(|element| {
        match element {
            Element::DenseNode(el_node) => {
                let mut node = ParsedNode::new(el_node.id() as OsmRef, el_node.lat(), el_node.lon());
                for (key, value) in el_node.tags() {
                    node.tags.insert(key.to_string(), value.to_string());
                }
                elem_count += 1;
                entity_storages.add_node(node, filterer);
            }
            Element::Way(el_way) => {
                let mut way = ParsedWay::new(el_way.id() as OsmRef);
                for (key, value) in el_way.tags() {
                    way.tags.insert(key.to_string(), value.to_string());
                }
                for r in el_way.refs() {
                    way.nodes_ref.push(r as OsmRef);
                }
                elem_count += 1;
                entity_storages.add_way(way, filterer);
            }
            Element::Relation(el_rel) => {
                let mut relation = ParsedRelation::new(el_rel.id() as u64);
                for (key, value) in el_rel.tags() {
                    relation.tags.insert(key.to_string(), value.to_string());
                }
                for way in el_rel
                    .members()
                    .filter(|member| member.member_type == RelMemberType::Way)
                {
                    relation.way_refs.push(ParsedRelationWay::new(
                        way.member_id as OsmRef,
                        way.role().unwrap() == "inner",
                    ));
                }
            }
            Element::Node(_) => panic!(),
        }
        if elem_count % 100_000 == 0 {
            entity_storages.print_short_stats();
        }
    })?;

    entity_storages.print_short_stats();

    Ok(entity_storages)
}

fn parse_osm_xml<P: AsRef<Path>>(input: P, filterer: Option<&Filterer>) -> Result<Parsed> {
    let mut entity_storages = Parsed::new();
    let mut elem_count = 0;
    let input_file = File::open(input.as_ref()).context(format!(
        "Failed to open {} for reading",
        input.as_ref().to_string_lossy()
    ))?;
    let mut parser = Reader::from_reader(BufReader::new(input_file));

    println!("Parsing XML");
    let mut buf = Vec::new();
    loop {
        let e = parser
            .read_event_into(&mut buf)
            .context("Failed to parse the input file")?;
        let mut on_elem = |start: BytesStart, have_subelements: bool| -> Result<()> {
            process_element(
                &mut parser,
                start.local_name().as_ref(),
                &mut start.attributes(),
                &mut entity_storages,
                have_subelements,
                filterer,
            )?;
            elem_count += 1;
            if elem_count % 100_000 == 0 {
                entity_storages.print_short_stats();
            }
            Ok(())
        };
        match e {
            Event::Eof => break,
            Event::Start(start) => on_elem(start, true)?,
            Event::Empty(start) => on_elem(start, false)?,
            _ => {}
        }
        // The official `quick-xml` examples suggests we do this to save memory.
        buf.clear();
    }

    entity_storages.print_full_stats();

    Ok(entity_storages)
}

fn process_element<R: BufRead>(
    parser: &mut Reader<R>,
    name: &[u8],
    attrs: &mut Attributes,
    entity_storages: &mut Parsed,
    have_subelements: bool,
    filterer: Option<&Filterer>,
) -> Result<()> {
    match name {
        b"node" => {
            let mut node = ParsedNode::new(
                get_id(parser, name, attrs)?,
                parse_required_attr(parser, name, attrs, b"lat")?,
                parse_required_attr(parser, name, attrs, b"lon")?,
            );
            if have_subelements {
                process_subelements(name, &mut node, process_node_subelement, parser)?;
            }
            entity_storages.add_node(node, filterer);
        }
        b"way" => {
            let mut way = ParsedWay::new(get_id(parser, name, attrs)?);
            if have_subelements {
                process_subelements(name, &mut way, process_way_subelement, parser)?;
            }
            entity_storages.add_way(way, filterer);
        }
        b"relation" => {
            let mut relation = ParsedRelation::new(get_id(parser, name, attrs)?);
            if have_subelements {
                process_subelements(name, &mut relation, process_relation_subelement, parser)?;
            }
            entity_storages.add_relation(relation, filterer);
        }
        _ => {}
    }
    Ok(())
}

fn process_subelements<E: Default, R: BufRead, F>(
    entity_name: &[u8],
    entity: &mut E,
    subelement_processor: F,
    parser: &mut Reader<R>,
) -> Result<()>
where
    F: Fn(&mut Reader<R>, &mut E, &[u8], &mut Attributes) -> Result<()>,
{
    let mut buf = Vec::new();
    loop {
        let e = parser.read_event_into(&mut buf).context(format!(
            "Failed to parse the input file when processing {}",
            ascii_name_as_str(entity_name)
        ))?;
        match e {
            Event::Eof => break,
            Event::End(end) if end.local_name().as_ref() == entity_name => break,
            Event::Start(start) | Event::Empty(start) => {
                subelement_processor(parser, entity, start.local_name().as_ref(), &mut start.attributes())?
            }
            _ => {}
        }
        buf.clear();
    }
    Ok(())
}

fn process_node_subelement<R: BufRead>(
    parser: &mut Reader<R>,
    node: &mut ParsedNode,
    sub_name: &[u8],
    sub_attrs: &mut Attributes,
) -> Result<()> {
    try_add_tag(parser, sub_name, sub_attrs, &mut node.tags).map(|_| ())
}

fn process_way_subelement<R: BufRead>(
    parser: &mut Reader<R>,
    way: &mut ParsedWay,
    sub_name: &[u8],
    sub_attrs: &mut Attributes,
) -> Result<()> {
    if try_add_tag(parser, sub_name, sub_attrs, &mut way.tags)? {
        return Ok(());
    }
    if sub_name == b"nd" {
        way.nodes_ref.push(get_ref(parser, sub_name, sub_attrs)?);
    }
    Ok(())
}

fn process_relation_subelement<R: BufRead>(
    parser: &mut Reader<R>,
    relation: &mut ParsedRelation,
    sub_name: &[u8],
    sub_attrs: &mut Attributes,
) -> Result<()> {
    if try_add_tag(parser, sub_name, sub_attrs, &mut relation.tags)? {
        return Ok(());
    }
    if sub_name == b"member" {
        let attr_type = get_required_attr(parser, sub_name, sub_attrs, b"type")?;

        match attr_type.as_ref() {
            "way" => {
                let osm_ref = get_ref(parser, sub_name, sub_attrs)?;
                let is_inner = get_required_attr(parser, sub_name, sub_attrs, b"role")? == "inner";

                relation.way_refs.push(ParsedRelationWay::new(osm_ref, is_inner));
            }
            "node" => {}
            &_ => {}
        }
    }
    Ok(())
}

fn ascii_name_as_str(elem_name: &[u8]) -> &str {
    std::str::from_utf8(elem_name).unwrap_or("N/A")
}

fn get_required_attr<'a, R: BufRead>(
    parser: &mut Reader<R>,
    elem_name: &[u8],
    attrs: &mut Attributes<'a>,
    attr_name: &[u8],
) -> Result<Cow<'a, str>> {
    for attr in attrs {
        let attr = attr?;
        if attr.key.local_name().as_ref() == attr_name {
            return Ok(attr.decode_and_unescape_value(parser)?);
        }
    }
    Err(anyhow!(
        "Element {} doesn't have required attribute: {}",
        ascii_name_as_str(elem_name),
        ascii_name_as_str(attr_name)
    ))
}

fn parse_required_attr<T, R: BufRead>(
    parser: &mut Reader<R>,
    elem_name: &[u8],
    attrs: &mut Attributes,
    attr_name: &[u8],
) -> Result<T>
where
    T: std::str::FromStr,
    T::Err: std::error::Error + Send + Sync + 'static,
{
    let value = get_required_attr(parser, elem_name, attrs, attr_name)?;

    let parsed_value = value.parse::<T>().context(format!(
        "Failed to parse the value of attribute {} ({}) for element {}",
        ascii_name_as_str(attr_name),
        value,
        ascii_name_as_str(elem_name)
    ))?;

    Ok(parsed_value)
}

fn try_add_tag<R: BufRead>(
    parser: &mut Reader<R>,
    elem_name: &[u8],
    attrs: &mut Attributes,
    tags: &mut ParsedTags,
) -> Result<bool> {
    if elem_name != b"tag" {
        return Ok(false);
    }
    let key = get_required_attr(parser, elem_name, attrs, b"k")?;
    let value = get_required_attr(parser, elem_name, attrs, b"v")?;
    tags.insert(key.to_string(), value.to_string());
    Ok(true)
}

fn get_id<R: BufRead>(parser: &mut Reader<R>, elem_name: &[u8], attrs: &mut Attributes) -> Result<u64> {
    match parse_required_attr(parser, elem_name, attrs, b"id") as Result<i64> {
        Ok(id) => Ok(id as u64),
        Err(err) => Err(err),
    }
}

fn get_ref<R: BufRead>(parser: &mut Reader<R>, elem_name: &[u8], attrs: &mut Attributes) -> Result<u64> {
    match parse_required_attr(parser, elem_name, attrs, b"ref") as Result<i64> {
        Ok(id) => Ok(id as u64),
        Err(err) => Err(err),
    }
}

pub(super) type OsmRef = u64;

pub(crate) type ParsedTags = BTreeMap<String, String>;

#[derive(Default)]
pub(super) struct ParsedNode {
    pub(super) id: OsmRef,
    pub(super) lat: f64,
    pub(super) lon: f64,
    pub(super) tags: ParsedTags,
}

impl ParsedNode {
    pub(super) fn new(id: OsmRef, lat: f64, lon: f64) -> ParsedNode {
        ParsedNode {
            id,
            lat,
            lon,
            tags: ParsedTags::new(),
        }
    }
}

impl coords::Coords for ParsedNode {
    fn lat(&self) -> f64 {
        self.lat
    }

    fn lon(&self) -> f64 {
        self.lon
    }
}

#[derive(Default)]
pub(super) struct ParsedWay {
    pub(super) id: OsmRef,
    pub(super) tags: ParsedTags,
    pub(super) nodes_ref: Vec<OsmRef>,
}

impl ParsedWay {
    fn new(id: OsmRef) -> ParsedWay {
        ParsedWay {
            id,
            tags: ParsedTags::new(),
            nodes_ref: Vec::new(),
        }
    }

    fn deduplicate_refs_pairs(&self) -> Vec<OsmRef> {
        if self.nodes_ref.is_empty() {
            return vec![];
        }

        let mut seen_node_pairs = HashSet::<(OsmRef, OsmRef)>::default();
        let mut refs_without_duplicates = vec![self.nodes_ref[0]];

        for idx in 1..self.nodes_ref.len() {
            let cur = self.nodes_ref[idx];
            let prev = self.nodes_ref[idx - 1];
            let node_pair = (cur, prev);
            if !seen_node_pairs.contains(&node_pair) && !seen_node_pairs.contains(&(prev, cur)) {
                seen_node_pairs.insert(node_pair);
                refs_without_duplicates.push(cur);
            }
        }

        return refs_without_duplicates;
    }
}

#[derive(Clone, Copy)]
pub(super) struct ParsedRelationWay {
    id: OsmRef,
    is_inner: bool,
}

impl ParsedRelationWay {
    pub(super) fn new(id: OsmRef, is_inner: bool) -> ParsedRelationWay {
        ParsedRelationWay { id, is_inner }
    }
}

pub(super) type ParsedPolygonId = u64;

#[derive(Default, Clone)]
pub(super) struct ParsedPolygon {
    pub(super) nodes: Vec<OsmRef>,
}

impl ParsedPolygon {
    pub(super) fn get_id(&self) -> ParsedPolygonId {
        let mut s = DefaultHasher::new();
        self.hash(&mut s);
        s.finish()
    }
}

impl Hash for ParsedPolygon {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.nodes.hash(state);
    }
}

#[derive(Default, Clone)]
pub(super) struct ParsedRelation {
    pub(super) id: OsmRef,
    pub(super) tags: ParsedTags,
    pub(super) way_refs: Vec<ParsedRelationWay>,
    pub(super) polygons: Vec<ParsedPolygonId>,
}

impl ParsedRelation {
    fn new(id: OsmRef) -> ParsedRelation {
        ParsedRelation {
            id,
            tags: ParsedTags::new(),
            way_refs: Vec::new(),
            polygons: Vec::new(),
        }
    }

    fn is_multipolygon(&self) -> bool {
        return self.tags.iter().any(|(k, v)| k == "type" && v == "multipolygon");
    }
}

pub(super) type LocalRef = usize;

#[derive(Copy, Clone)]
pub(super) struct IndexedNode<'a> {
    pub(super) node: &'a ParsedNode,
    pub(super) local_id: LocalRef,
}

pub(super) struct IndexedWay<'a> {
    pub(super) way: &'a ParsedWay,
    pub(super) nodes_ref: Vec<LocalRef>,
}

pub(super) struct IndexedPolygon {
    pub(super) nodes_ref: Vec<LocalRef>,
}

pub(super) struct IndexedRelation<'a> {
    pub(super) relation: &'a ParsedRelation,
    pub(super) polygons_ref: Vec<LocalRef>,
}

pub(super) struct Indexed<'a> {
    pub(super) nodes: Vec<IndexedNode<'a>>,
    pub(super) ways: Vec<IndexedWay<'a>>,
    pub(super) polygons: Vec<IndexedPolygon>,
    pub(super) relations: Vec<IndexedRelation<'a>>,

    pub(super) nodes_ref: HashMap<OsmRef, LocalRef>,
    pub(super) polygons_ref: HashMap<OsmRef, LocalRef>,
}

impl<'a> Indexed<'a> {
    pub(super) fn new() -> Indexed<'static> {
        Indexed {
            nodes: Vec::new(),
            ways: Vec::new(),
            polygons: Vec::new(),
            relations: Vec::new(),

            nodes_ref: HashMap::new(),
            polygons_ref: HashMap::new(),
        }
    }

    pub(self) fn add_node_ref(&mut self, node: &'a ParsedNode) -> LocalRef {
        match self.nodes_ref.get(&node.id) {
            Some(local_id) => *local_id,
            None => {
                let node_ref = IndexedNode {
                    node,
                    local_id: self.nodes.len(),
                };

                self.nodes_ref.insert(node.id, node_ref.local_id);
                self.nodes.push(node_ref);

                node_ref.local_id
            }
        }
    }

    pub(self) fn ensure_node_ref(&mut self, osm_node_id: OsmRef, parsed: &'a Parsed) -> Option<LocalRef> {
        match self.nodes_ref.get(&osm_node_id) {
            Some(local_id) => Some(*local_id),
            None => {
                if let Some(node) = parsed.nodes.get(&osm_node_id) {
                    Some(self.add_node_ref(node))
                } else {
                    None
                }
            }
        }
    }

    pub(self) fn build_nodes_index(&mut self, parsed: &'a Parsed) {
        for (_, node) in parsed.nodes.iter() {
            if node.tags.is_empty() {
                continue;
            }
            self.add_node_ref(node);
        }
    }

    pub(self) fn build_ways_index(&mut self, parsed: &'a Parsed) {
        for (_, way) in parsed.ways.iter() {
            if way.tags.is_empty() {
                continue;
            }
            let mut way_ref = IndexedWay {
                way,
                nodes_ref: Vec::new(),
            };

            for node_osm_id in way.nodes_ref.iter() {
                way_ref
                    .nodes_ref
                    .push(self.ensure_node_ref(*node_osm_id, parsed).unwrap());
            }

            self.ways.push(way_ref);
        }
    }

    pub(self) fn ensure_polygon(&mut self, polygon_id: &ParsedPolygonId, parsed: &'a Parsed) -> LocalRef {
        match self.polygons_ref.get(&polygon_id) {
            Some(local_id) => *local_id,
            None => {
                let polygon = parsed.polygons.get(polygon_id).unwrap();
                let local_id = self.polygons.len();
                let indexed_poly = IndexedPolygon {
                    nodes_ref: polygon
                        .nodes
                        .iter()
                        .map(|osm_node_id| self.ensure_node_ref(*osm_node_id, parsed).unwrap())
                        .collect(),
                };

                self.polygons.push(indexed_poly);
                self.polygons_ref.insert(*polygon_id, local_id);

                local_id
            }
        }
    }

    pub(self) fn build_relations_index(&mut self, parsed: &'a Parsed) {
        for (_, relation) in parsed.relations.iter() {
            if relation.tags.is_empty() {
                continue;
            }

            let indexed_relation = IndexedRelation {
                relation,
                polygons_ref: relation
                    .polygons
                    .iter()
                    .map(|polygon_id| self.ensure_polygon(polygon_id, parsed))
                    .collect(),
            };

            self.relations.push(indexed_relation);
        }
    }

    pub(super) fn build_from(&mut self, parsed: &'a Parsed) {
        self.nodes.clear();
        self.ways.clear();
        self.nodes_ref.clear();
        self.polygons_ref.clear();
        self.build_relations_index(parsed);
        self.build_ways_index(parsed);
        self.build_nodes_index(parsed);
    }

    pub(super) fn print_stats(&self) {
        println!(
            "Indexed {} nodes, {} ways, {} polygons, {} relations",
            self.nodes.len(),
            self.ways.len(),
            self.polygons.len(),
            self.relations.len()
        );
    }
}

pub(super) struct Parsed {
    pub(super) nodes: HashMap<OsmRef, ParsedNode>,
    pub(super) ways: HashMap<OsmRef, ParsedWay>,
    pub(super) relations: HashMap<OsmRef, ParsedRelation>,

    pub(super) polygons: HashMap<ParsedPolygonId, ParsedPolygon>,

    pub(super) count_tags: usize,
    pub(super) count_notag_nodes: usize,
    pub(super) count_notag_ways: usize,
    pub(super) count_norefs_ways: usize,
    pub(super) count_notag_relations: usize,
    pub(super) count_norefs_relations: usize,
    pub(super) count_deduplicated_ways: usize,

    pub(super) count_filtered_nodes: usize,
    pub(super) count_filtered_ways: usize,
    pub(super) count_filtered_relations: usize,
    pub(super) count_nomultipolygons: usize,
    pub(super) count_nopolygons_relations: usize,
    pub(super) count_filtered_tags: usize,
}

impl Parsed {
    pub(super) fn new() -> Parsed {
        Parsed {
            nodes: HashMap::new(),
            ways: HashMap::new(),
            relations: HashMap::new(),

            polygons: HashMap::new(),

            count_tags: 0,
            count_notag_nodes: 0,
            count_notag_ways: 0,
            count_norefs_ways: 0,
            count_notag_relations: 0,
            count_norefs_relations: 0,
            count_deduplicated_ways: 0,

            count_filtered_nodes: 0,
            count_filtered_ways: 0,
            count_filtered_relations: 0,
            count_nomultipolygons: 0,
            count_nopolygons_relations: 0,
            count_filtered_tags: 0,
        }
    }

    pub(super) fn add_node(&mut self, node: ParsedNode, filterer: Option<&Filterer>) {
        if node.tags.is_empty() {
            self.count_notag_nodes += 1;
        } else if filterer.is_some() {
            let filtered_tags = filterer.unwrap().filter_tags(vec![ObjectType::Node], &node.tags);
            if filtered_tags.len() != node.tags.len() {
                let mut filtered_node = ParsedNode::new(node.id, node.lat, node.lon);

                if filtered_tags.is_empty() {
                    self.count_filtered_nodes += 1;
                } else {
                    self.count_filtered_tags += node.tags.len() - filtered_tags.len();
                    self.count_tags += filtered_tags.len();
                }

                filtered_node.tags = filtered_tags;
                /* insert truncated node: may needed for ways or relations */
                self.nodes.insert(node.id, filtered_node);
                return;
            }
        }

        self.count_tags += node.tags.len();
        self.nodes.insert(node.id, node);
    }

    pub(super) fn add_way(&mut self, way: ParsedWay, filterer: Option<&Filterer>) {
        let mut add_way_simple = |way: ParsedWay| {
            if way.nodes_ref.len() == 0 {
                self.count_norefs_ways += 1;
            }

            self.count_tags += way.tags.len();
            let node_refs = way.deduplicate_refs_pairs();
            if node_refs.len() != way.nodes_ref.len() {
                let mut clear_way = ParsedWay::new(way.id);

                clear_way.tags = way.tags.clone();
                clear_way.nodes_ref = node_refs;
                self.count_deduplicated_ways += 1;
                self.ways.insert(way.id, clear_way);
            } else {
                self.ways.insert(way.id, way);
            }
        };

        if way.tags.len() == 0 {
            self.count_notag_ways += 1;
        } else if filterer.is_some() {
            let filtered_tags = filterer
                .unwrap()
                .filter_tags(vec![ObjectType::Way, ObjectType::Area], &way.tags);

            if filtered_tags.len() != way.tags.len() {
                let mut filtered_way = ParsedWay::new(way.id);
                filtered_way.nodes_ref = way.nodes_ref.clone();

                if filtered_tags.len() == 0 {
                    self.count_filtered_ways += 1;
                } else {
                    self.count_filtered_tags += way.tags.len() - filtered_tags.len();
                }

                filtered_way.tags = filtered_tags;
                add_way_simple(filtered_way);
                return;
            }
        }
        add_way_simple(way);
    }

    pub(self) fn add_multipolygon(&mut self, relation: ParsedRelation) {
        let (outer_ways, inner_ways) = self.extract_ways(&relation);

        if !outer_ways.is_empty() {
            let oxpolygons = collect_polygons::ways_to_polygons(outer_ways);

            // Without outer polygons, there is no inner
            if !oxpolygons.is_empty() {
                let mut poly_relation = relation.clone();

                if !inner_ways.is_empty() {
                    for poly in collect_polygons::ways_to_polygons(inner_ways) {
                        let ppoly = ParsedPolygon { nodes: poly.nodes() };
                        let ppoly_id = ppoly.get_id();

                        poly_relation.polygons.push(ppoly_id);
                        self.polygons.insert(ppoly_id, ppoly);
                    }
                }

                for poly in oxpolygons {
                    let ppoly = ParsedPolygon { nodes: poly.nodes() };
                    let ppoly_id = ppoly.get_id();

                    poly_relation.polygons.push(ppoly_id);
                    self.polygons.insert(ppoly_id, ppoly);
                }

                self.count_tags += poly_relation.tags.len();
                self.relations.insert(relation.id, poly_relation);
            } else {
                self.count_nopolygons_relations += 1;
            }
        } else {
            self.count_nopolygons_relations += 1;
        }
    }

    pub(super) fn add_relation(&mut self, relation: ParsedRelation, filterer: Option<&Filterer>) {
        if relation.tags.len() == 0 {
            self.count_notag_relations += 1;
            return;
        }

        if !relation.is_multipolygon() {
            self.count_nomultipolygons += 1;
            /* do nothing: relation is filtered */
            return;
        }

        if filterer.is_some() {
            let filtered_tags = filterer.unwrap().filter_tags(vec![ObjectType::Area], &relation.tags);

            if filtered_tags.is_empty() {
                self.count_filtered_relations += 1;
                return;
            }

            if filtered_tags.len() != relation.tags.len() {
                let mut filtered_relation = ParsedRelation::new(relation.id);

                self.count_filtered_tags += relation.tags.len() - filtered_tags.len();
                filtered_relation.tags = filtered_tags;

                self.add_multipolygon(relation);
                return;
            }
        }

        if relation.tags.is_empty() {
            self.count_filtered_relations += 1;
            return;
        }

        if relation.way_refs.is_empty() {
            /* do nothing: relation is filtered */
            self.count_norefs_relations += 1;
            return;
        }

        self.add_multipolygon(relation);
    }

    pub(super) fn print_short_stats(&self) {
        println!(
            "Contains {} nodes, {} ways, {} relations, {} polygons",
            self.nodes.len(),
            self.ways.len(),
            self.relations.len(),
            self.polygons.len(),
        );
    }

    pub(super) fn print_full_stats(&self) {
        println!(
            "Contains {} nodes ({} truncated), {} ways ({} truncated), {} relations ({} filtered, {} no multipolygons, {} without polygons), {} polygons, {} tags ({} filtered)",
            self.nodes.len(),
            self.count_filtered_nodes,
            self.ways.len(),
            self.count_filtered_ways,
            self.relations.len(),
            self.count_filtered_relations,
            self.count_nomultipolygons,
            self.count_nopolygons_relations,
            self.polygons.len(),
            self.count_tags,
            self.count_filtered_tags,
        );
        println!(
            "Without tags: {} nodes, {} ways, {} relations",
            self.count_notag_nodes, self.count_notag_ways, self.count_norefs_relations,
        );
        println!(
            "Without refs: {} ways, {} relations",
            self.count_norefs_ways, self.count_norefs_relations
        );
        println!("Deduplicated node pairs in ways: {}", self.count_deduplicated_ways);
    }

    pub(super) fn extract_ways(
        &self,
        relation: &ParsedRelation,
    ) -> (Vec<collect_polygons::Way>, Vec<collect_polygons::Way>) {
        let mut not_found: Vec<OsmRef> = Vec::new();
        let mut outer_ways = Vec::new();
        let mut inner_ways = Vec::new();

        for way_ref in relation.way_refs.iter() {
            match self.ways.get(&way_ref.id) {
                Some(way) => {
                    let first_node = self.nodes.get(way.nodes_ref.first().unwrap()).unwrap();
                    let first_node_pos = collect_polygons::NodePos::new(first_node.lat, first_node.lon);
                    let last_node = self.nodes.get(way.nodes_ref.last().unwrap()).unwrap();
                    let last_node_pos = collect_polygons::NodePos::new(last_node.lat, last_node.lon);

                    if !way_ref.is_inner {
                        outer_ways.push(collect_polygons::Way::new(
                            way.nodes_ref.clone(),
                            first_node_pos,
                            last_node_pos,
                        ));
                    } else {
                        inner_ways.push(collect_polygons::Way::new(
                            way.nodes_ref.clone(),
                            first_node_pos,
                            last_node_pos,
                        ));
                    }
                }
                None => {
                    not_found.push(way_ref.id);
                }
            }
        }

        if !not_found.is_empty() {
            println!(
                "relation #{}: {}/{} ways not found",
                relation.id,
                not_found.len(),
                relation.way_refs.len()
            );
        }

        (outer_ways, inner_ways)
    }
}
