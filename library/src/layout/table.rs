use arrayvec::ArrayVec;

use crate::layout::{AlignNode, GridLayouter, Sizing, TrackSizings};
use crate::prelude::*;

/// # Table
/// A table of items.
///
/// Tables are used to arrange content in cells. Cells can contain arbitrary
/// content, including multiple paragraphs and are specified in row-major order.
/// Because tables are just grids with configurable cell properties, refer to
/// the [grid documentation]($func/grid) for more information on how to size the
/// table tracks.
///
/// ## Example
/// ```example
/// #table(
///   columns: (1fr, auto, auto),
///   inset: 10pt,
///   align: horizon,
///   [], [*Area*], [*Parameters*],
///   image("cylinder.svg"),
///   $ pi h (D^2 - d^2) / 4 $,
///   [
///     $h$: height \
///     $D$: outer radius \
///     $d$: inner radius
///   ],
///   image("tetrahedron.svg"),
///   $ sqrt(2) / 12 a^3 $,
///   [$a$: edge length]
/// )
/// ```
///
/// ## Parameters
/// - cells: `Content` (positional, variadic)
///   The contents of the table cells.
///
/// - rows: `TrackSizings` (named)
///   Defines the row sizes.
///   See the [grid documentation]($func/grid) for more information on track
///   sizing.
///
/// - columns: `TrackSizings` (named)
///   Defines the column sizes.
///   See the [grid documentation]($func/grid) for more information on track
///   sizing.
///
/// ## Category
/// layout
#[func]
#[capable(Layout)]
#[derive(Debug, Hash)]
pub struct TableNode {
    /// Defines sizing for content rows and columns.
    pub tracks: Axes<Vec<Sizing>>,
    /// The content to be arranged in the table.
    pub cells: Vec<Content>,
}

#[node]
impl TableNode {
    /// How to fill the cells.
    ///
    /// This can be a color or a function that returns a color. The function is
    /// passed the cell's column and row index, starting at zero. This can be
    /// used to implement striped tables.
    ///
    /// ```example
    /// #table(
    ///   fill: (col, _) => if calc.odd(col) { luma(240) } else { white },
    ///   align: (col, row) =>
    ///     if row == 0 { center }
    ///     else if col == 0 { left }
    ///     else { right },
    ///   columns: 4,
    ///   [], [*Q1*], [*Q2*], [*Q3*],
    ///   [Revenue:], [1000 €], [2000 €], [3000 €],
    ///   [Expenses:], [500 €], [1000 €], [1500 €],
    ///   [Profit:], [500 €], [1000 €], [1500 €],
    /// )
    /// ```
    #[property(referenced)]
    pub const FILL: Celled<Option<Paint>> = Celled::Value(None);

    /// How to align the cell's content.
    ///
    /// This can either be a single alignment or a function that returns an
    /// alignment. The function is passed the cell's column and row index,
    /// starting at zero. If set to `{auto}`, the outer alignment is used.
    #[property(referenced)]
    pub const ALIGN: Celled<Smart<Axes<Option<GenAlign>>>> = Celled::Value(Smart::Auto);

    /// How to stroke the cells.
    ///
    /// This can be a color, a stroke width, both, or `{none}` to disable
    /// the stroke.
    #[property(referenced)]
    pub const STROKE: Celled<Lines> =
        Celled::Value(Lines::splat(Some(PartialStroke::default())));

    /// How much to pad the cells's content.
    ///
    /// The default is `{5pt}`.
    #[property(referenced)]
    pub const INSET: Celled<Rel<Length>> = Celled::Value(Abs::pt(5.0).into());

    fn construct(_: &Vm, args: &mut Args) -> SourceResult<Content> {
        let TrackSizings(columns) = args.named("columns")?.unwrap_or_default();
        let TrackSizings(rows) = args.named("rows")?.unwrap_or_default();
        Ok(Self {
            tracks: Axes::new(columns, rows),
            cells: args.all()?,
        }
        .pack())
    }

    fn field(&self, name: &str) -> Option<Value> {
        match name {
            "columns" => Some(Sizing::encode_slice(&self.tracks.x)),
            "rows" => Some(Sizing::encode_slice(&self.tracks.y)),
            "cells" => Some(Value::Array(
                self.cells.iter().cloned().map(Value::Content).collect(),
            )),
            _ => None,
        }
    }
}

impl Layout for TableNode {
    fn layout(
        &self,
        vt: &mut Vt,
        styles: StyleChain,
        regions: Regions,
    ) -> SourceResult<Fragment> {
        if self.cells.is_empty() {
            return Ok(Fragment::frame(Frame::new(Size::zero())));
        }

        let cols = self.tracks.x.len().max(1);
        let rows = (self.cells.len() as f64 / cols as f64).ceil() as usize;

        // Resolve properties.
        let inset = styles.get(Self::INSET);
        let align = styles.get(Self::ALIGN);
        let fill = styles.get(Self::FILL);
        let strokes = Strokes::resolve(vt, styles, cols, rows)?;

        // Apply alignments and insets.
        let cells: Vec<_> = self
            .cells
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, child)| {
                let x = i % cols;
                let y = i / cols;
                let mut child = child.padded(Sides::splat(inset.resolve(vt, x, y)?));
                if let Smart::Custom(alignment) = align.resolve(vt, x, y)? {
                    child = child.styled(AlignNode::ALIGNS, alignment)
                }
                Ok(child)
            })
            .collect::<SourceResult<_>>()?;

        // Prepare grid layout.
        let layouter = GridLayouter::new(
            vt,
            self.tracks.as_deref(),
            Axes::splat(&[]),
            &cells,
            regions,
            styles,
        );

        // Measure the columns and layout the grid row-by-row.
        let mut layout = layouter.layout()?;

        // Render strokes and fills.
        for (frame, &(start, ref rows)) in layout.fragment.iter_mut().zip(&layout.rows) {
            // Render horizontal lines.
            for (k, dy) in offsets(rows).enumerate() {
                let mut dx = Abs::zero();
                let segments = strokes.get(Axis::X, start + k);
                for (i, (&col, &stroke)) in layout.cols.iter().zip(segments).enumerate() {
                    let Some(stroke) = stroke else {
                        dx += col;
                        continue;
                    };

                    let left = strokes.get(Axis::Y, i);
                    let mut lpad = if k == 0 {
                        left[start + k].map_or(Abs::zero(), |s| s.thickness)
                    } else if k == rows.len() {
                        left[start + k - 1].map_or(Abs::zero(), |s| s.thickness)
                    } else {
                        left[start + k - 1]
                            .map_or(Abs::zero(), |s| s.thickness)
                            .max(left[start + k].map_or(Abs::zero(), |s| s.thickness))
                    };

                    let right = strokes.get(Axis::Y, i + 1);
                    let mut rpad = if k == 0 {
                        right[start + k].map_or(Abs::zero(), |s| s.thickness)
                    } else if k == rows.len() {
                        right[start + k - 1].map_or(Abs::zero(), |s| s.thickness)
                    } else {
                        right[start + k - 1]
                            .map_or(Abs::zero(), |s| s.thickness)
                            .max(right[start + k].map_or(Abs::zero(), |s| s.thickness))
                    };

                    if i > 0 {
                        lpad /= 2.0;
                    }
                    if i + 1 < layout.cols.len() {
                        rpad /= 2.0;
                    }

                    let origin = dx + lpad;
                    let target = dx + col - rpad;
                    let delta = Point::with_x(target - origin);
                    let hline = Geometry::Line(delta).stroked(stroke);
                    let mut y = dy;
                    if k == 0 {
                        y += stroke.thickness / 2.0;
                    } else if k == rows.len() {
                        y -= stroke.thickness / 2.0;
                    }
                    frame.prepend(Point::new(origin, y), Element::Shape(hline));
                    dx += col;
                }
            }

            // Render vertical lines.
            for (k, dx) in offsets(&layout.cols).enumerate() {
                let mut dy = Abs::zero();
                let segments = &strokes.get(Axis::Y, k)[start..start + rows.len()];
                let mut continuation = None;
                for (i, (&row, &stroke)) in rows.iter().zip(segments).enumerate() {
                    let mut origin = dy;
                    let target = dy + row;
                    dy += row;

                    let Some(stroke) = stroke else { continue };
                    if let Some(prev) = continuation.take() {
                        origin = prev;
                    }

                    if segments.get(i + 1) == Some(&Some(stroke)) {
                        continuation = Some(origin);
                    } else {
                        let mut x = dx;
                        if k == 0 {
                            x += stroke.thickness / 2.0;
                        } else if k == layout.cols.len() {
                            x -= stroke.thickness / 2.0;
                        }
                        let delta = Point::with_y(target - origin);
                        let vline = Geometry::Line(delta).stroked(stroke);
                        frame.prepend(Point::new(x, origin), Element::Shape(vline));
                    }
                }
            }

            // Render fills.
            let mut dx = Abs::zero();
            for (x, &col) in layout.cols.iter().enumerate() {
                let mut dy = Abs::zero();
                for (y, &row) in rows.iter().enumerate() {
                    let y = start + y;
                    if let Some(fill) = fill.resolve(vt, x, y)? {
                        let pos = Point::new(dx, dy);
                        let size = Size::new(col, row);
                        let rect = Geometry::Rect(size).filled(fill);
                        frame.prepend(pos, Element::Shape(rect));
                    }
                    dy += row;
                }
                dx += col;
            }
        }

        Ok(layout.fragment)
    }
}

/// Turn a slice of extents into an iterator of offsets before, in between, and
/// after the extents, e.g. [10mm, 5mm] -> [0mm, 10mm, 15mm].
fn offsets(extents: &[Abs]) -> impl Iterator<Item = Abs> + '_ {
    let mut offset = Abs::zero();
    std::iter::once(Abs::zero())
        .chain(extents.iter().copied())
        .map(move |extent| {
            offset += extent;
            offset
        })
}

/// A value that can be configured per cell.
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Celled<T> {
    /// A bare value, the same for all cells.
    Value(T),
    /// A closure mapping from cell coordinates to a value.
    Func(Func),
}

impl<T: Cast + Clone> Celled<T> {
    /// Resolve the value based on the cell position.
    pub fn resolve(&self, vt: &Vt, x: usize, y: usize) -> SourceResult<T> {
        Ok(match self {
            Self::Value(value) => value.clone(),
            Self::Func(func) => {
                let args =
                    Args::new(func.span(), [Value::Int(x as i64), Value::Int(y as i64)]);
                func.call_detached(vt.world(), args)?.cast().at(func.span())?
            }
        })
    }
}

impl<T: Cast> Cast for Celled<T> {
    fn is(value: &Value) -> bool {
        matches!(value, Value::Func(_)) || T::is(value)
    }

    fn cast(value: Value) -> StrResult<Self> {
        match value {
            Value::Func(v) => Ok(Self::Func(v)),
            v if T::is(&v) => Ok(Self::Value(T::cast(v)?)),
            v => <Self as Cast>::error(v),
        }
    }

    fn describe() -> CastInfo {
        T::describe() + CastInfo::Type("function")
    }
}

/// Resolved strokes for all separators.
#[derive(Debug)]
struct Strokes {
    strokes: Axes<Vec<Option<Stroke>>>,
    tracks: Axes<usize>,
}

impl Strokes {
    /// Resolve the line configuration for all cells.
    fn resolve(
        vt: &Vt,
        styles: StyleChain,
        cols: usize,
        rows: usize,
    ) -> SourceResult<Self> {
        let tracks = Axes::new(cols, rows);
        let lines = {
            let celled = styles.get(TableNode::STROKE);
            let mut prepared: Vec<Lines<Abs>> = vec![];
            for y in 0..rows {
                for x in 0..cols {
                    prepared.push(celled.resolve(vt, x, y)?.resolve(styles));
                }
            }
            move |axis, main, cross, side, outside| {
                let pos: Axes<usize> = match axis {
                    Axis::X => Axes::new(main, cross),
                    Axis::Y => Axes::new(cross, main),
                };
                prepared[pos.y * cols + pos.x].get(side, outside)
            }
        };

        let mut strokes = Axes::splat(vec![]);
        for axis in [Axis::X, Axis::Y] {
            let strokes = strokes.get_mut(axis);
            let other = axis.other();
            let dir = other.dir(true);
            for cross in 0..=tracks.get(other) {
                for main in 0..tracks.get(axis) {
                    strokes.push(if cross == 0 {
                        mix(lines(axis, main, cross, dir.start(), true))
                    } else if cross < tracks.get(other) {
                        let first = lines(axis, main, cross, dir.start(), false);
                        let second = lines(axis, main, cross - 1, dir.end(), false);
                        mix(first.chain(second))
                    } else {
                        mix(lines(axis, main, cross - 1, dir.end(), true))
                    });
                }
            }
        }

        Ok(Self { strokes, tracks })
    }

    /// Get the strokes for the k-th separator line on the given axis.
    fn get(&self, axis: Axis, k: usize) -> &[Option<Stroke>] {
        let stride = self.tracks.get(axis);
        &self.strokes.get_ref(axis)[k * stride..(k + 1) * stride]
    }

    // / Get the maximum thickness of the two vertical line adjacent to the
    // / k-th horizontal line track in column `x`.
    // fn thickness(&self, x: usize, k: usize) -> Abs {
    //     self.get(Axis::X, k)
    //         .iter()
    //         .map(|s| s.map_or(Abs::zero(), |s| s.thickness))
    //         .max()
    //         .unwrap_or_default()
    // }
}

/// Combine strokes by priority.
fn mix(
    iter: impl Iterator<Item = (usize, Option<PartialStroke<Abs>>)>,
) -> Option<Stroke> {
    let mut vec: ArrayVec<_, 4> = iter.collect();
    vec.sort_by_key(|&(p, _)| p);
    vec.into_iter()
        .map(|(_, stroke)| stroke)
        .reduce(|first, second| first.fold(second))?
        .map(PartialStroke::unwrap_or_default)
}

/// Line configuration for a cell.
#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub struct Lines<T = Length> {
    inside: Option<Option<PartialStroke<T>>>,
    outside: Option<Option<PartialStroke<T>>>,
    sides: Sides<Option<Option<PartialStroke<T>>>>,
}

impl<T> Lines<T> {
    /// Equal lines on all sides.
    fn splat(value: Option<PartialStroke<T>>) -> Self
    where
        T: Clone,
    {
        Self {
            inside: None,
            outside: None,
            sides: Sides::splat(Some(value)),
        }
    }
}

impl Lines<Abs> {
    /// Get the prioritized values for the given side.
    fn get(
        &self,
        side: Side,
        outside: bool,
    ) -> impl Iterator<Item = (usize, Option<PartialStroke<Abs>>)> {
        let first = if outside { self.outside } else { self.inside };
        let second = self.sides.get(side);
        [first, second]
            .into_iter()
            .enumerate()
            .filter_map(|(p, s)| s.map(|s| (p, s)))
    }
}

impl Resolve for Lines {
    type Output = Lines<Abs>;

    fn resolve(self, styles: StyleChain) -> Self::Output {
        Lines {
            sides: self.sides.resolve(styles),
            inside: self.inside.resolve(styles),
            outside: self.outside.resolve(styles),
        }
    }
}

castable! {
    Lines,
    stroke: Option<PartialStroke> => Self::splat(stroke),
    mut dict: Dict => {
        let mut take = |key| dict.take(key).ok().map(Value::cast).transpose();

        let rest = take("rest")?;
        let x = take("x")?.or(rest);
        let y = take("y")?.or(rest);
        let lines = Self {
            sides: Sides {
                left: take("left")?.or(y),
                top: take("top")?.or(x),
                right: take("right")?.or(y),
                bottom: take("bottom")?.or(x),
            },
            inside: take("inside")?,
            outside: take("outside")?,
        };
        dict.finish(&[
            "left", "top", "right", "bottom",
            "inside", "outside", "x", "y", "rest",
        ])?;
        lines
    },
}
