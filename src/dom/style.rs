//! Styles for text and pages.

use std::rc::Rc;

use fontdock::{fallback, FallbackTree, FontStyle, FontVariant, FontWeight, FontWidth};

use crate::geom::{Insets, Size};
use crate::length::{Length, ScaleLength};
use crate::paper::{Paper, PaperClass, PAPER_A4};

/// Defines properties of pages and text.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Style {
    /// The style for text.
    pub text: Rc<TextStyle>,
    /// The style for pages.
    pub page: Rc<PageStyle>,
}

/// Defines which fonts to use and how to space text.
#[derive(Debug, Clone, PartialEq)]
pub struct TextStyle {
    /// A tree of font family names and generic class names.
    pub fallback: Rc<FallbackTree>,
    /// The selected font variant.
    pub variant: FontVariant,
    /// Whether the bolder toggle is active or inactive. This determines
    /// whether the next `*` adds or removes font weight.
    pub bolder: bool,
    /// Whether the italic toggle is active or inactive. This determines
    /// whether the next `_` makes italic or non-italic.
    pub italic: bool,
    /// The base font size.
    pub base_font_size: f64,
    /// The font scale to apply on the base font size.
    pub font_scale: f64,
    /// The word spacing (as a multiple of the font size).
    pub word_spacing_scale: f64,
    /// The line spacing (as a multiple of the font size).
    pub line_spacing_scale: f64,
    /// The paragraphs spacing (as a multiple of the font size).
    pub paragraph_spacing_scale: f64,
}

impl TextStyle {
    /// The scaled font size.
    pub fn font_size(&self) -> f64 {
        self.base_font_size * self.font_scale
    }

    /// The absolute word spacing.
    pub fn word_spacing(&self) -> f64 {
        self.word_spacing_scale * self.font_size()
    }

    /// The absolute line spacing.
    pub fn line_spacing(&self) -> f64 {
        (self.line_spacing_scale - 1.0) * self.font_size()
    }

    /// The absolute paragraph spacing.
    pub fn paragraph_spacing(&self) -> f64 {
        (self.paragraph_spacing_scale - 1.0) * self.font_size()
    }
}

impl Default for TextStyle {
    fn default() -> Self {
        Self {
            fallback: Rc::new(fallback! {
                list: ["sans-serif"],
                classes: {
                    "serif" => ["source serif pro", "noto serif"],
                    "sans-serif" => ["source sans pro", "noto sans"],
                    "monospace" => ["source code pro", "noto sans mono"],
                    "math" => ["latin modern math", "serif"],
                },
                base: [
                    "source sans pro", "noto sans", "segoe ui emoji",
                    "noto emoji", "latin modern math",
                ],
            }),
            variant: FontVariant {
                style: FontStyle::Normal,
                weight: FontWeight(400),
                width: FontWidth::Medium,
            },
            bolder: false,
            italic: false,
            base_font_size: Length::pt(11.0).as_raw(),
            font_scale: 1.0,
            word_spacing_scale: 0.25,
            line_spacing_scale: 1.2,
            paragraph_spacing_scale: 1.5,
        }
    }
}

/// Defines the size and margins of a page.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct PageStyle {
    /// The class of this page.
    pub class: PaperClass,
    /// The width and height of the page.
    pub size: Size,
    /// The amount of white space on each side. If a side is set to `None`, the
    /// default for the paper class is used.
    pub margins: [Option<ScaleLength>; 4],
}

impl PageStyle {
    /// The default page style for the given paper.
    pub fn new(paper: Paper) -> Self {
        Self {
            class: paper.class,
            size: paper.size(),
            margins: [None; 4],
        }
    }

    /// The absolute margins.
    pub fn margins(&self) -> Insets {
        let size = self.size;
        let default = self.class.default_margins();
        Insets {
            x0: -self.margins[0].unwrap_or(default[0]).raw_scaled(size.width),
            y0: -self.margins[1].unwrap_or(default[1]).raw_scaled(size.height),
            x1: -self.margins[2].unwrap_or(default[2]).raw_scaled(size.width),
            y1: -self.margins[3].unwrap_or(default[3]).raw_scaled(size.height),
        }
    }
}

impl Default for PageStyle {
    fn default() -> Self {
        Self::new(PAPER_A4)
    }
}