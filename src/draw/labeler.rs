use crate::draw::font::text_placer::TextPlacer;
use crate::draw::icon::Icon;
use crate::draw::icon_cache::IconCache;
use crate::draw::labelable::Labelable;
use crate::draw::tile_pixels::TilePixels;
use crate::mapcss::styler::LabelStyle;

#[derive(Default)]
pub struct Labeler {
    text_placer: TextPlacer,
}

impl Labeler {
    pub fn label_entity<E>(
        &self,
        entity: &E,
        style: &LabelStyle,
        scale: f64,
        icon_cache: &IconCache,
        pixels: &mut TilePixels,
    ) where
        E: Labelable,
    {
        let succeeded = {
            if let Some(y_offset) = self.label_with_icon(entity, style, scale, icon_cache, pixels) {
                self.label_with_text(entity, style, scale, y_offset, pixels)
            } else {
                false
            }
        };

        pixels.bump_label_generation(succeeded);
    }

    fn label_with_icon(
        &self,
        entity: &impl Labelable,
        style: &LabelStyle,
        scale: f64,
        icon_cache: &IconCache,
        pixels: &mut TilePixels,
    ) -> Option<usize> {
        let icon_name = match style.icon_image {
            Some(ref icon_name) => icon_name,
            _ => return Some(0),
        };

        let read_icon_cache = icon_cache.open_read_session(icon_name);

        if let Some(Some(icon)) = read_icon_cache.get(icon_name) {
            let (center_x, center_y) = match entity.get_label_position(scale) {
                Some(center) => center,
                _ => return Some(0),
            };
            if self.draw_icon(icon, center_x, center_y, pixels) {
                Some(icon.height / 2)
            } else {
                None
            }
        } else {
            Some(0)
        }
    }

    fn label_with_text<E>(
        &self,
        entity: &E,
        style: &LabelStyle,
        scale: f64,
        y_offset: usize,
        pixels: &mut TilePixels,
    ) -> bool
    where
        E: Labelable,
    {
        if let Some(ref text_style) = style.text_style {
            self.text_placer.place(entity, text_style, scale, y_offset, pixels)
        } else {
            true
        }
    }

    fn draw_icon(&self, icon: &Icon, center_x: f64, center_y: f64, pixels: &mut TilePixels) -> bool {
        let get_start_coord = |coord, dimension| (coord - (dimension as f64 / 2.0)) as i32;

        let start_x = get_start_coord(center_x, icon.width);
        let start_y = get_start_coord(center_y, icon.height);

        for x in 0..icon.width {
            for y in 0..icon.height {
                if !pixels.set_label_pixel(start_x + x as i32, start_y + y as i32, &icon.get(x, y)) {
                    return false;
                }
            }
        }

        true
    }
}
