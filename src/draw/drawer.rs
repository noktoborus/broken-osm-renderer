use crate::draw::fill::{fill_contour, Filler};
use crate::draw::icon_cache::IconCache;
use crate::draw::labelable::TiledEntitySource;
use crate::draw::labeler::Labeler;
use crate::draw::line::draw_lines;
use crate::draw::png_writer::rgb_triples_to_png;
use crate::draw::point_pairs::PointPairCollection;
use crate::draw::tile_pixels::{RgbTriples, TilePixels};
use crate::mapcss::styler::{Style, StyledEntities, Styler};
use crate::tile::tile::Tile;
use anyhow::Result;
use std::path::Path;

pub struct Drawer {
    icon_cache: IconCache,
    labeler: Labeler,
}

#[derive(Clone, Eq, PartialEq, Hash)]
enum DrawType {
    Fill,
    Stroke,
    Casing,
}

pub struct TileRenderedPixels {
    pub triples: RgbTriples,
    pub dimension: usize,
}

impl Drawer {
    pub fn new(base_path: &Path) -> Drawer {
        Drawer {
            icon_cache: IconCache::new(base_path),
            labeler: Labeler::default(),
        }
    }

    pub fn draw(
        &self,
        styled_entities: &StyledEntities,
        pixels: &mut TilePixels,
        tile: &Tile,
        scale: f64,
        styler: &Styler,
    ) -> Result<Vec<u8>> {
        let rendered_pixels = self.draw_to_pixels(styled_entities, pixels, tile, scale, styler);

        {
            let _m = crate::perf_stats::measure("RGB triples to PNG");
            rgb_triples_to_png(
                &rendered_pixels.triples,
                rendered_pixels.dimension,
                rendered_pixels.dimension,
            )
        }
    }

    pub fn draw_to_pixels(
        &self,
        styled_entities: &StyledEntities,
        pixels: &mut TilePixels,
        tile: &Tile,
        scale: f64,
        styler: &Styler,
    ) -> TileRenderedPixels {
        {
            let _m = crate::perf_stats::measure("Resetting TilePixels");
            pixels.reset(&styler.canvas_fill_color);
        }

        let draw_areas_with_type = |pixels: &mut TilePixels, draw_type| {
            styled_entities.styled.iter().for_each(|(entity, style)| {
                self.draw_geometry(pixels, &entity.get_tiled(tile), style, scale, styler, draw_type)
            });
        };

        {
            let _m = crate::perf_stats::measure("Fill areas");
            draw_areas_with_type(pixels, &DrawType::Fill);
        }

        {
            let _m = crate::perf_stats::measure("Draw casing");
            draw_areas_with_type(pixels, &DrawType::Casing);
        }

        {
            let _m = crate::perf_stats::measure("Draw stroke");
            draw_areas_with_type(pixels, &DrawType::Stroke);
        }

        {
            let _m = crate::perf_stats::measure("Blend after areas");
            pixels.blend_unfinished_pixels(false);
        }

        {
            let _m = crate::perf_stats::measure("Draw labels");
            self.draw_labels(pixels, styled_entities, scale, tile);
        }

        {
            let _m = crate::perf_stats::measure("Blend after labels");
            pixels.blend_unfinished_pixels(true);
        }

        TileRenderedPixels {
            triples: pixels.to_rgb_triples(),
            dimension: pixels.dimension(),
        }
    }

    fn draw_labels(&self, pixels: &mut TilePixels, styled_entities: &StyledEntities, scale: f64, tile: &Tile) {
        {
            let _m = crate::perf_stats::measure("Label areas");
            for (entity, labelstyle) in &styled_entities.labeled {
                self.labeler
                    .label_entity(&entity.get_tiled(tile), labelstyle, scale, &self.icon_cache, pixels);
            }
        }
    }

    fn draw_geometry<'e, A>(
        &self,
        pixels: &mut TilePixels,
        relation: &'e A,
        style: &Style,
        scale: f64,
        styler: &Styler,
        draw_type: &DrawType,
    ) where
        A: PointPairCollection<'e>,
    {
        let points = relation.to_point_pairs(scale);

        let scale_dashes =
            |dashes: &Option<Vec<f64>>| dashes.as_ref().map(|nums| nums.iter().map(|x| x * scale).collect());

        match *draw_type {
            DrawType::Fill => {
                if let Some(ref color) = style.fill_color {
                    fill_contour(points, &Filler::Color(color), style.fill_opacity, pixels);
                } else if let Some(ref icon_name) = style.fill_image {
                    let read_icon_cache = self.icon_cache.open_read_session(icon_name);
                    if let Some(Some(icon)) = read_icon_cache.get(icon_name) {
                        fill_contour(points, &Filler::Image(icon), style.fill_opacity, pixels);
                    }
                }
            }
            DrawType::Casing => {
                if let Some(color) = style.casing_color.as_ref() {
                    draw_lines(
                        points,
                        style.casing_width * scale,
                        color,
                        1.0,
                        &scale_dashes(&style.casing_dashes),
                        style.casing_line_cap,
                        styler.use_caps_for_dashes,
                        pixels,
                    );
                }
            }
            DrawType::Stroke => {
                if let Some(color) = style.color.as_ref() {
                    draw_lines(
                        points,
                        scale * style.width,
                        color,
                        style.opacity,
                        &scale_dashes(&style.dashes),
                        style.line_cap,
                        styler.use_caps_for_dashes,
                        pixels,
                    );
                }
            }
        }

        pixels.bump_generation();
    }
}
