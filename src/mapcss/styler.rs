use crate::draw::point::Point;
use crate::geodata::reader::GeoEntity;
use crate::geodata::reader::{Multipolygon, Node, OsmArea, OsmEntities, OsmEntity, Way};
use crate::mapcss::color::{from_color_name, Color};
use crate::mapcss::parser::*;
use crate::mapcss::style_cache::{CacheableEntity, StyleCache};
use indexmap::IndexMap;
use std::cmp::Ordering;
use std::fmt::Display;
use std::sync::Arc;
use std::sync::RwLock;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Copy)]
pub enum LineCap {
    Butt,
    Round,
    Square,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TextPosition {
    Center,
    Line,
}

pub fn is_non_trivial_cap(line_cap: &LineCap) -> bool {
    matches!(line_cap, LineCap::Square | LineCap::Round)
}

pub trait StyleableEntity {
    fn default_text_position(&self) -> TextPosition;
    fn default_z_index(&self) -> f64;
    fn matches_object_type(&self, object_type: &ObjectType) -> bool;
}

#[derive(Debug, Clone)]
pub struct TextStyle {
    pub text: String,
    pub text_color: Color,
    pub text_position: TextPosition,
    pub font_size: f64,
}

#[derive(Debug, Clone)]
pub struct LabelStyle {
    /// greater value of rule Id that used to fill this style
    rule_id: RuleId,
    pub icon_image: Option<String>,
    pub text_style: Option<TextStyle>,
}

impl Display for LabelStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(icon_image) = &self.icon_image {
            write!(f, ", icon-image: {:?}", icon_image)?;
        }
        if let Some(text_style) = &self.text_style {
            write!(
                f,
                ", text: {:?}, font-size: {}, text-position: {:?}, text-color: {:?}",
                text_style.text, text_style.font_size, text_style.text_position, text_style.text_color
            )?;
        }
        write!(f, "")
    }
}

#[derive(Debug)]
pub struct Style {
    /// greater value of rule Id that used to fill this style
    pub rule_id: RuleId,

    pub layer: i64,
    pub z_index: f64,

    pub is_foreground_fill: bool,

    pub fill_color: Option<Color>,
    pub fill_image: Option<String>,
    pub fill_opacity: f64,

    pub color: Option<Color>,
    pub opacity: f64,
    pub width: f64,
    pub dashes: Option<Vec<f64>>,
    pub line_cap: LineCap,

    pub casing_color: Option<Color>,
    pub casing_width: f64,
    pub casing_dashes: Option<Vec<f64>>,
    pub casing_line_cap: LineCap,
}

impl Display for Style {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "layer: {}, z_index: {}, is_foreground_fill: {}",
            self.layer, self.z_index, self.is_foreground_fill
        )?;

        if self.fill_color.is_some() || self.fill_image.is_some() {
            write!(f, ", fill-opacity: {}", self.fill_opacity)?;

            if let Some(fill_color) = &self.fill_color {
                write!(f, ", fill-color: {:?}", fill_color)?;
            }
            if let Some(fill_image) = &self.fill_image {
                write!(f, ", fill-image: {:?}", fill_image)?;
            }
        }

        if let Some(color) = &self.color {
            write!(
                f,
                ", opacity: {}, color: {:?}, width: {}, line-cap: {:?}",
                self.opacity, color, self.width, self.line_cap
            )?;

            if let Some(dashes) = &self.dashes {
                write!(f, ", dashes: {:?}", dashes)?;
            }
        }

        if let Some(casing_color) = &self.casing_color {
            write!(
                f,
                ", casing-width: {}, casing-color: {:?}, casing-line_cap: {:?}",
                self.casing_width, casing_color, self.casing_line_cap
            )?;

            if let Some(casing_dashes) = &self.casing_dashes {
                write!(f, ", casing_dashes: {:?}", casing_dashes)?;
            }
        }

        write!(f, "")
    }
}

pub struct Styler {
    pub canvas_fill_color: Option<Color>,
    pub use_caps_for_dashes: bool,

    casing_width_multiplier: f64,
    font_size_multiplier: Option<f64>,
    rules: Vec<Rule>,

    style_cache: RwLock<StyleCache>,
}

#[derive(Clone)]
pub enum OsmEntityType<'a, 'wr>
where
    'a: 'wr,
{
    Node(&'wr Node<'a>),
    Way(&'wr Way<'a>),
    Multipolygon(&'wr Multipolygon<'a>),
}

impl<'a, 'wr> std::fmt::Debug for OsmEntityType<'a, 'wr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OsmEntityType::Node(n) => write!(f, "node/{}", n.global_id()),
            OsmEntityType::Way(w) => write!(f, "way/{}", w.global_id()),
            OsmEntityType::Multipolygon(mp) => write!(f, "relation/{}", mp.global_id()),
        }
    }
}

pub trait TileRelative {
    fn get_points(&self) -> Vec<Point>;
    fn get_relation(&self) -> Vec<Vec<Point>>;
}

pub struct StyledEntities<'a, 'wr> {
    pub styled: Vec<(OsmEntityType<'a, 'wr>, Arc<Style>)>,
    pub labeled: Vec<(OsmEntityType<'a, 'wr>, Arc<LabelStyle>)>,
}

impl<'a, 'wr> StyledEntities<'a, 'wr> {
    pub fn new(styler: &Styler, entities: &'a OsmEntities<'a>, zoom: u8) -> Self {
        let mut styled = Vec::new();
        let mut labeled = Vec::new();

        let _m = crate::perf_stats::measure("Styling");

        {
            let _m = crate::perf_stats::measure("Style ways");
            let mut iter = entities
                .ways
                .iter()
                .flat_map(|way| styler.style_entity(way, entities, zoom));

            while let Some((element, style, labelstyle)) = iter.next() {
                if let Some(labelstyle) = labelstyle {
                    labeled.push((OsmEntityType::Way(element), labelstyle));
                }
                if let Some(style) = style {
                    styled.push((OsmEntityType::Way(element), style));
                }
            }
        }

        {
            let _m = crate::perf_stats::measure("Style multipolygons");
            let mut iter = entities
                .multipolygons
                .iter()
                .flat_map(|multipolygon| styler.style_entity(multipolygon, entities, zoom));

            while let Some((element, style, labelstyle)) = iter.next() {
                if let Some(labelstyle) = labelstyle {
                    labeled.push((OsmEntityType::Multipolygon(element), labelstyle));
                }
                if let Some(style) = style {
                    styled.push((OsmEntityType::Multipolygon(element), style));
                }
            }
        }

        {
            let _m = crate::perf_stats::measure("Sorting styled areas");
            styled.sort_by(|(_, a), (_, b)| compare_styled_entities(&a, &b));
        }

        {
            let _m = crate::perf_stats::measure("Style nodes");
            entities
                .nodes
                .iter()
                .flat_map(|node| styler.style_entity(node, entities, zoom))
                .filter(|(_, _, labelstyle)| labelstyle.is_some())
                .map(|(entity, _, labelstyle)| (OsmEntityType::Node(entity), labelstyle.unwrap()))
                .for_each(|x| labeled.push(x));
        };

        {
            let _m = crate::perf_stats::measure("Sorting labels");
            labeled.sort_by(|(_, a), (_, b)| compare_styled_entities_labels(&a, &b));
        }

        Self { styled, labeled }
    }

    pub fn is_empty(&self) -> bool {
        self.styled.is_empty() && self.labeled.is_empty()
    }
}

impl Styler {
    pub fn new(rules: Vec<Rule>, font_size_multiplier: Option<f64>) -> Styler {
        let canvas_fill_color = extract_canvas_fill_color(&rules);
        let style_cache = StyleCache::new(&rules);

        Styler {
            use_caps_for_dashes: true,
            canvas_fill_color,
            casing_width_multiplier: 2.0,
            font_size_multiplier,
            rules,
            style_cache: RwLock::new(style_cache),
        }
    }

    pub fn style<'a>(&'a self, entities: &'a OsmEntities<'a>, zoom: u8) -> StyledEntities {
        StyledEntities::new(self, entities, zoom)
    }

    pub fn style_entity<'e, 'wp, A>(
        &self,
        entity: &'wp A,
        entities: &OsmEntities<'_>,
        zoom: u8,
    ) -> Vec<(&'wp A, Option<Arc<Style>>, Option<Arc<LabelStyle>>)>
    where
        A: StyleableEntity + GeoEntity + CacheableEntity + OsmEntity<'e>,
    {
        let mut styles = Vec::new();
        let _m = crate::perf_stats::measure("Style entity");

        {
            let _m = crate::perf_stats::measure("Read style cache");

            let read_cache = self.style_cache.read().unwrap();
            if let Some(styles) = read_cache.get(entity, zoom) {
                return styles
                    .iter()
                    .map(|(style, labelstyle)| (entity, style.clone(), labelstyle.clone()))
                    .collect();
            }
        }

        {
            let _m = crate::perf_stats::measure("Probe MapCSS selectors");

            let all_property_maps = self.get_property_maps(entity, entities, zoom);
            let base_layer = all_property_maps
                .iter()
                .find(|kvp| *kvp.0 == BASE_LAYER_NAME)
                .map(|kvp| kvp.1);

            for (layer, prop_map) in &all_property_maps {
                if *layer != "*" {
                    styles.push((
                        property_map_to_style(prop_map, base_layer, self.casing_width_multiplier, entity)
                            .map(|x| Arc::new(x)),
                        property_map_to_labelstyle(prop_map, &self.font_size_multiplier, entity).map(|x| Arc::new(x)),
                    ))
                }
            }
        }

        {
            let _m = crate::perf_stats::measure("Write style cache");

            let mut write_cache = self.style_cache.write().unwrap();

            write_cache.insert(entity, zoom, styles.clone());
        }

        styles
            .iter()
            .map(|(style, labelstyle)| (entity, style.clone(), labelstyle.clone()))
            .collect()
    }

    fn get_property_maps<'r, 'e, A>(&'r self, area: &A, entities: &OsmEntities<'_>, zoom: u8) -> LayerToPropertyMap<'r>
    where
        A: StyleableEntity + GeoEntity + OsmEntity<'e>,
    {
        let mut result: LayerToPropertyMap<'r> = IndexMap::new();

        for rule in &self.rules {
            for sel in rule
                .selectors
                .iter()
                .filter(|x| entity_matches(area, entities, x, zoom))
            {
                let layer_id = get_layer_id(sel);

                let update_layer = |layer: &mut PropertyMap<'r>| {
                    for prop in &rule.properties {
                        layer.insert(prop.name.clone(), (rule.rule_id, &prop.value));
                    }
                };

                {
                    // Can't use result.entry(...).or_insert_with(...) because we need to immutably
                    // borrow the result to compute the default value in or_insert_with(), and the
                    // map is already borrowed as mutable when we call entry().
                    if !result.contains_key(layer_id) {
                        let parent_layer = result.get("*").cloned().unwrap_or_default();
                        result.insert(layer_id, parent_layer);
                    }

                    update_layer(result.get_mut(layer_id).unwrap());
                }

                if layer_id == "*" {
                    for (_, v) in result.iter_mut().filter(|&(k, _)| k != &"*") {
                        update_layer(v);
                    }
                }
            }
        }

        result
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(clippy::float_cmp))]
fn compare_styled_entities(a_style: &Arc<Style>, b_style: &Arc<Style>) -> Ordering {
    let (a_layer, b_layer) = (a_style.layer, b_style.layer);

    if a_layer != b_layer {
        return a_layer.cmp(&b_layer);
    }

    if a_style.is_foreground_fill != b_style.is_foreground_fill {
        return a_style.is_foreground_fill.cmp(&b_style.is_foreground_fill);
    }

    if a_style.z_index != b_style.z_index {
        return a_style.z_index.partial_cmp(&b_style.z_index).unwrap();
    }

    a_style.rule_id.cmp(&b_style.rule_id)
}

#[cfg_attr(feature = "cargo-clippy", allow(clippy::float_cmp))]
fn compare_styled_entities_labels(a_labelstyle: &Arc<LabelStyle>, b_labelstyle: &Arc<LabelStyle>) -> Ordering {
    a_labelstyle.rule_id.cmp(&b_labelstyle.rule_id)
}

type LayerToPropertyMap<'r> = IndexMap<&'r str, PropertyMap<'r>>;
type PropertyMap<'r> = IndexMap<String, (RuleId, &'r PropertyValue)>;

fn property_map_to_style<'r, 'e, E>(
    current_layer_map: &'r PropertyMap<'r>,
    base_layer_map: Option<&'r PropertyMap<'r>>,
    casing_width_multiplier: f64,
    osm_entity: &E,
) -> Option<Style>
where
    E: StyleableEntity + OsmEntity<'e>,
{
    let warn = |prop_map: &'r PropertyMap<'r>, prop_name, msg| {
        if let Some(val) = prop_map.get(prop_name) {
            eprintln!(
                "Entity #{}, property \"{}\" (value {:?}): {}",
                osm_entity.global_id(),
                prop_name,
                val,
                msg
            );
        }
    };

    let get_rule_id = |prop_name| current_layer_map.get(prop_name).map(|(rule_id, _)| *rule_id);

    let get_color = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Color(color)) => Some(color.clone()),
        Some(&PropertyValue::Identifier(id)) => {
            let color = from_color_name(id.as_str());
            if color.is_none() {
                warn(current_layer_map, prop_name, "unknown color");
            }
            color
        }
        _ => {
            warn(current_layer_map, prop_name, "expected a valid color");
            None
        }
    };

    let get_num = |prop_map: &'r PropertyMap<'r>, prop_name| match prop_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Numbers(nums)) if nums.len() == 1 => Some(nums[0]),
        _ => {
            warn(prop_map, prop_name, "expected a number");
            None
        }
    };

    let get_id = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Identifier(id)) => Some(id.as_str()),
        _ => {
            warn(current_layer_map, prop_name, "expected an identifier");
            None
        }
    };

    let get_string = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Identifier(id)) => Some(id.to_string()),
        Some(&PropertyValue::String(str)) => Some(str.to_string()),
        _ => {
            warn(current_layer_map, prop_name, "expected a string");
            None
        }
    };

    let get_line_cap = |prop_name| match get_id(prop_name) {
        Some("none") | Some("butt") => Some(LineCap::Butt),
        Some("round") => Some(LineCap::Round),
        Some("square") => Some(LineCap::Square),
        _ => {
            warn(current_layer_map, prop_name, "unknown line cap value");
            None
        }
    };

    let get_dashes = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Numbers(nums)) => Some(nums.clone()),
        _ => {
            warn(current_layer_map, prop_name, "expected a sequence of numbers");
            None
        }
    };

    let layer = osm_entity
        .tags()
        .get_by_key("layer")
        .and_then(|x| x.parse::<i64>().ok())
        .unwrap_or(0);
    let z_index = get_num(current_layer_map, "z-index").unwrap_or(osm_entity.default_z_index());

    let is_foreground_fill = !matches!(current_layer_map.get("fill-position").map(|(_, prop)| prop), Some(&PropertyValue::Identifier(id)) if id == "background");

    let width = get_num(current_layer_map, "width");

    let base_width_for_casing = width
        .or_else(|| base_layer_map.and_then(|prop_map| get_num(prop_map, "width")))
        .unwrap_or_default();
    let casing_only_width = match current_layer_map.get("casing-width").map(|(_, prop)| prop) {
        Some(&PropertyValue::Numbers(nums)) if nums.len() == 1 => Some(nums[0]),
        Some(&&PropertyValue::WidthDelta(num)) => Some(base_width_for_casing + num),
        _ => {
            warn(
                current_layer_map,
                "casing-width",
                "expected a number or an eval(...) statement",
            );
            None
        }
    }
    .unwrap_or(1.0);

    let full_casing_width = base_width_for_casing + casing_width_multiplier * casing_only_width;
    let color = get_color("color");
    let fill_color = get_color("fill-color");
    let casing_color = get_color("casing-color");
    let fill_image = get_string("fill-image");

    if color.is_some() || fill_color.is_some() || casing_color.is_some() || fill_image.is_some() {
        let rule_id = [
            get_rule_id("color"),
            get_rule_id("fill-color"),
            get_rule_id("casing-color"),
            get_rule_id("fill-image"),
        ]
        .iter()
        .filter(|x| x.is_some())
        .map(|x| x.unwrap())
        .max()
        .unwrap();

        Some(Style {
            rule_id,

            layer,
            z_index,

            is_foreground_fill,

            color,
            fill_color,
            opacity: get_num(current_layer_map, "opacity").unwrap_or(1.0),
            fill_opacity: get_num(current_layer_map, "fill-opacity").unwrap_or(1.0),

            width: width.unwrap_or(1.0),
            dashes: get_dashes("dashes"),
            line_cap: get_line_cap("linecap").unwrap_or(LineCap::Butt),

            casing_color,
            casing_width: full_casing_width,
            casing_dashes: get_dashes("casing-dashes"),
            casing_line_cap: get_line_cap("casing-linecap").unwrap_or(LineCap::Butt),

            fill_image,
        })
    } else {
        None
    }
}

fn property_map_to_labelstyle<'r, 'e, E>(
    current_layer_map: &'r PropertyMap<'r>,
    font_size_multiplier: &Option<f64>,
    osm_entity: &E,
) -> Option<LabelStyle>
where
    E: StyleableEntity + OsmEntity<'e>,
{
    let warn = |prop_map: &'r PropertyMap<'r>, prop_name, msg| {
        if let Some(val) = prop_map.get(prop_name) {
            eprintln!(
                "Entity #{}, property \"{}\" (value {:?}): {}",
                osm_entity.global_id(),
                prop_name,
                val,
                msg
            );
        }
    };

    let get_rule_id = |prop_name| current_layer_map.get(prop_name).map(|(rule_id, _)| *rule_id);

    let get_color = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Color(color)) => Some(color.clone()),
        Some(&PropertyValue::Identifier(id)) => {
            let color = from_color_name(id.as_str());
            if color.is_none() {
                warn(current_layer_map, prop_name, "unknown color");
            }
            color
        }
        _ => {
            warn(current_layer_map, prop_name, "expected a valid color");
            None
        }
    };

    let get_num = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Numbers(nums)) if nums.len() == 1 => Some(nums[0]),
        _ => {
            warn(current_layer_map, prop_name, "expected a number");
            None
        }
    };

    let get_id = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Identifier(id)) => Some(id.as_str()),
        _ => {
            warn(current_layer_map, prop_name, "expected an identifier");
            None
        }
    };

    let get_string = |prop_name| match current_layer_map.get(prop_name).map(|(_, prop)| prop) {
        Some(&PropertyValue::Identifier(id)) => Some(id.to_string()),
        Some(&PropertyValue::String(str)) => Some(str.to_string()),
        _ => {
            warn(current_layer_map, prop_name, "expected a string");
            None
        }
    };

    let get_text_position = |prop_name| match get_id(prop_name) {
        Some("center") => Some(TextPosition::Center),
        Some("line") => Some(TextPosition::Line),
        _ => {
            warn(current_layer_map, prop_name, "unknown text position type");
            None
        }
    };

    let text = get_string("text").and_then(|tagkey| osm_entity.tags().get_by_key(tagkey.as_ref()));
    let font_size = get_num("font-size").map(|x| x * font_size_multiplier.unwrap_or(1.0));
    let icon_image = get_string("icon-image");
    let text_style = match font_size {
        Some(font_size) => text.map(|text| TextStyle {
            text: text.to_string(),
            text_color: get_color("text-color").unwrap_or(Color { r: 0, g: 0, b: 0 }),
            text_position: get_text_position("text-position").unwrap_or(osm_entity.default_text_position()),
            font_size,
        }),
        None => None,
    };

    if icon_image.is_some() || text_style.is_some() {
        let rule_id = [get_rule_id("icon-image"), get_rule_id("text")]
            .iter()
            .filter(|x| x.is_some())
            .map(|x| x.unwrap())
            .max()
            .unwrap();

        Some(LabelStyle {
            rule_id,
            text_style,
            icon_image,
        })
    } else {
        None
    }
}

fn extract_canvas_fill_color(rules: &[Rule]) -> Option<Color> {
    for r in rules {
        for selector in &r.selectors {
            if let ObjectType::Canvas = selector.object_type {
                for prop in r.properties.iter().filter(|x| x.name == "fill-color") {
                    if let PropertyValue::Color(ref color) = prop.value {
                        return Some(color.clone());
                    }
                }
            }
        }
    }
    None
}

fn matches_by_tags<'e, E>(entity: &E, test: &Test) -> bool
where
    E: OsmEntity<'e>,
{
    let tags = entity.tags();

    let is_true_value = |x| x == "yes" || x == "true" || x == "1";

    match *test {
        Test::Unary {
            ref tag_name,
            ref test_type,
        } => {
            let tag_val = tags.get_by_key(tag_name);
            match *test_type {
                UnaryTestType::Exists => tag_val.is_some(),
                UnaryTestType::NotExists => tag_val.is_none(),
                UnaryTestType::True => matches!(tag_val, Some(x) if is_true_value(x)),
                UnaryTestType::False => !matches!(tag_val, Some(x) if is_true_value(x)),
            }
        }
        Test::BinaryStringCompare {
            ref tag_name,
            ref value,
            ref test_type,
        } => {
            let tag_val = tags.get_by_key(tag_name);
            match *test_type {
                BinaryStringTestType::Equal => tag_val == Some(value),
                BinaryStringTestType::NotEqual => tag_val != Some(value),
            }
        }
        Test::BinaryNumericCompare {
            ref tag_name,
            ref value,
            ref test_type,
        } => {
            let tag_val = match tags.get_by_key(tag_name).map(str::parse::<f64>) {
                Some(Ok(x)) => x,
                _ => return false,
            };
            match *test_type {
                BinaryNumericTestType::Less => tag_val < *value,
                BinaryNumericTestType::LessOrEqual => tag_val <= *value,
                BinaryNumericTestType::Greater => tag_val > *value,
                BinaryNumericTestType::GreaterOrEqual => tag_val >= *value,
            }
        }
    }
}

fn parent_matches<'e, E>(entity: &E, entities: &OsmEntities<'_>, parent_selector: &ParentSelector) -> bool
where
    E: OsmEntity<'e>,
{
    if entity.is_node() {
        let my_node_id = entity.global_id();

        for way in entities
            .ways
            .iter()
            .filter(|x| x.matches_object_type(&parent_selector.object_type))
        {
            for i in 0..way.node_count() {
                if way.get_node(i).global_id() == my_node_id {
                    if parent_selector.tests.iter().all(|test| matches_by_tags(way, test)) {
                        return true;
                    }
                }
            }
        }
        // TODO: matches to relations
    } else if entity.is_way() {
        // TODO: matches to relations
    }
    return false;
}

fn entity_matches<'e, A>(area: &A, entities: &OsmEntities<'_>, selector: &Selector, zoom: u8) -> bool
where
    A: StyleableEntity + GeoEntity + OsmEntity<'e>,
{
    if let Some(min_zoom) = selector.min_zoom {
        if zoom < min_zoom {
            return false;
        }
    }

    if let Some(max_zoom) = selector.max_zoom {
        if zoom > max_zoom {
            return false;
        }
    }

    if !area.matches_object_type(&selector.object_type) {
        return false;
    }

    if !selector.tests.iter().all(|x| matches_by_tags(area, x)) {
        return false;
    }

    if let Some(parent_selector) = &selector.parent_selector {
        if !parent_matches(area, entities, parent_selector) {
            return false;
        }
    }

    true
}

fn get_layer_id(selector: &Selector) -> &str {
    match selector.layer_id {
        Some(ref id) => id,
        None => BASE_LAYER_NAME,
    }
}

const BASE_LAYER_NAME: &str = "default";

impl<'a> StyleableEntity for Node<'a> {
    fn default_z_index(&self) -> f64 {
        4.0
    }

    fn matches_object_type(&self, object_type: &ObjectType) -> bool {
        matches!(*object_type, ObjectType::Node)
    }

    fn default_text_position(&self) -> TextPosition {
        TextPosition::Center
    }
}

impl<'a> StyleableEntity for Way<'a> {
    fn default_z_index(&self) -> f64 {
        if self.is_closed() {
            1.0
        } else {
            3.0
        }
    }

    fn matches_object_type(&self, object_type: &ObjectType) -> bool {
        match *object_type {
            ObjectType::Way => true,
            ObjectType::Area => self.is_closed(),
            _ => false,
        }
    }

    fn default_text_position(&self) -> TextPosition {
        if self.is_closed() {
            TextPosition::Center
        } else {
            TextPosition::Line
        }
    }
}

impl<'a> StyleableEntity for Multipolygon<'a> {
    fn default_z_index(&self) -> f64 {
        if self.is_closed() {
            1.0
        } else {
            3.0
        }
    }

    fn matches_object_type(&self, object_type: &ObjectType) -> bool {
        match *object_type {
            ObjectType::Way => false,
            ObjectType::Area => true,
            _ => false,
        }
    }

    fn default_text_position(&self) -> TextPosition {
        if self.is_closed() {
            TextPosition::Center
        } else {
            TextPosition::Line
        }
    }
}
