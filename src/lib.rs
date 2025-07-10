use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use anyhow::{Context, bail, ensure};
use rust_decimal::Decimal;

mod svg;

struct Category<D, O> {
    display: D,
    ordering: O,
}

struct Value<V, E> {
    value: V,
    extra: E,
}

impl<V, E> Value<V, E> {}

#[derive(Clone, Copy, Debug)]
enum Location {
    Horizontal,
    Vertical,
}

impl Location {
    fn invert(self) -> Location {
        match self {
            Location::Horizontal => Location::Vertical,
            Location::Vertical => Location::Horizontal,
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Horizontal => f.write_str("Horizontal"),
            Location::Vertical => f.write_str("Vertical"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum LocationOrdering {
    Before,
    After,
    Both,
}

impl LocationOrdering {
    fn instances(self) -> u8 {
        match self {
            LocationOrdering::Before => 1,
            LocationOrdering::After => 1,
            LocationOrdering::Both => 2,
        }
    }
}

impl Display for LocationOrdering {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocationOrdering::Before => f.write_str("Before"),
            LocationOrdering::After => f.write_str("After"),
            LocationOrdering::Both => f.write_str("Both"),
        }
    }
}

// this chart is categorical so it won't be plotting that much anyway
// this can be used for:
// - regular beam/column chart
// - gantt chart
// - waterfall chart
// - categories are rect, multirect, candlestick, boxplot, etc, etc

// the amount of fields is diabolical, but is kept private.
struct RectChart<D, O, V, E, const N: usize> {
    data: Vec<(Category<D, O>, Value<V, E>)>,
    tooltip: Box<dyn Fn(D, O, V, E) -> String>,
    category_location: Location,
    category_style: Box<dyn Fn(&D, &O) -> RectStyle>,
    display_category: Box<dyn Fn(&D, &O) -> String>,
    plot_category: Box<dyn Fn(&O) -> u32>,
    tooltip_values: Box<dyn Fn(&V) -> [String; N]>,
    plot_vaues: Box<dyn Fn(&V) -> [Decimal; N]>,
    plot_shape: Box<dyn Fn(&D, &O, &V, &E) -> [ShapeInfo; N]>, // if invalid, eg EndRect before start or multiple start with no end, validation will catch it
    // these don't need to be calculated dynamically as we already have all the data by now
    // don't need lines to seperate categories...
    value_lines: Vec<(Decimal, LineStyle)>,

    categories_gutter_proportion: Proportion,
    // categories_margin_proportion: Proportion,
    // categories_padding_proportion: Proportion,
    categories_name_proportion: Proportion,
    categoires_name_location: LocationOrdering,

    values_name_proportion: Proportion,
    values_name_location: LocationOrdering,

    horizontal_border_style: Option<LineStyle>,
    vertical_border_style: Option<LineStyle>,
    value_line_style: LineStyle,

    chart_title: String, // need a better location ... and also specify whether in/out of chart
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
struct Proportion(Decimal);
impl Proportion {
    fn remainder(self) -> Proportion {
        Proportion(Decimal::ONE - self.0)
    }
    fn new(d: Decimal) -> anyhow::Result<Proportion> {
        ensure!(
            d >= Decimal::ZERO && d <= Decimal::ONE,
            "Invalid proportion of `{d}` - expected [0, 1]"
        );
        Ok(Proportion(d))
    }
    const ZERO: Proportion = Proportion(Decimal::ZERO);
    const ONE: Proportion = Proportion(Decimal::ONE);
}

impl<D, O, V, E, const N: usize> RectChart<D, O, V, E, N> {
    fn render(&self) -> anyhow::Result<SvgChart> {
        ensure!(
            N != 0,
            "Cannot render a chart where values per category is 0"
        );

        let categories_proportion = Proportion::new(
            self.categories_gutter_proportion.0
                + self.values_name_proportion.0
                    * Decimal::from(self.values_name_location.instances()),
        )
        .with_context(|| format!("Too much {} proportion", self.category_location))?;

        let values_proportion = Proportion::new(
            self.categories_name_proportion.0
                * Decimal::from(self.categoires_name_location.instances()),
        )
        .with_context(|| format!("Too much {} proportion", self.category_location.invert()))?;

        // find numer of categories - will be used for queries proportional to the categories
        let category_set = self
            .data
            .iter()
            .map(|(c, _)| (self.plot_category)(&c.ordering))
            .collect::<BTreeSet<_>>();
        ensure!(
            category_set.len() == self.data.len(),
            "Duplicate categories!"
        );

        // find min and max values
        let Some(min) = self
            .data
            .iter()
            .filter_map(|(_, v)| (self.plot_vaues)(&v.value).into_iter().min())
            .min()
        else {
            bail!("Cannot render a chart with empty dataset");
        };

        let Some(max) = self
            .data
            .iter()
            .filter_map(|(_, v)| (self.plot_vaues)(&v.value).into_iter().max())
            .max()
        else {
            bail!("Cannot render a chart with empty dataset");
        };

        // this will be used for queries proportional to the values
        let values_range = max - min;

        let lines_min = self.value_lines.iter().map(|(v, _)| v).min().copied();
        let lines_max = self.value_lines.iter().map(|(v, _)| v).max().copied();

        let value_plot_min = lines_min.unwrap_or(min).min(min);
        let value_plot_max = lines_max.unwrap_or(max).max(max);

        let value_plot_range = value_plot_max - value_plot_min;
        // figure out the total size we need

        let extent = {
            match self.category_location {
                // column chart
                Location::Horizontal => Point {
                    x: Decimal::from(10_000),
                    y: value_plot_range / values_proportion.remainder().0,
                },
                // beam char
                Location::Vertical => Point {
                    x: value_plot_range / values_proportion.remainder().0,
                    y: Decimal::from(10_000),
                },
            }
        };

        let mut chart = SvgChart {
            extent,
            labels: Vec::new(),
            rects: Vec::new(),
            lines: Vec::new(),
        };

        // add all rects and labels
        // TODO: text wrapping. just render on one line for now
        for (c, v) in &self.data {
            // organize into ordered plot categories
            // enumerate it
            // so that we know how to plot
        }

        // add value lines and labels
        // TODO: text wrapping. just render on one line for now
        for (v, s) in &self.value_lines {
            // can render in any random order
            // make sure to chuck the text in the right place
        }

        Ok(chart)
    }
}

enum ShapeInfo {
    Line,
    StartRect,
    EndRect,
}

struct TextStyle {
    color: String,
    // later font, etc
    // size? proportional?
}

struct RectStyle {
    color: String,
    // later shading, gradients
    // border, fill, etc
    // corner radius :)
    // SHADOW??? :(
}

struct LineStyle {
    color: String,
    drawing: LineDrawingStyle,
}

enum LineDrawingStyle {
    Solid,
    Dashed,
    Dotted,
}

// another chart which is like a scatter will need to be more efficient
// struct PointChart<P, O, I, N, T>

struct Point {
    x: Decimal,
    y: Decimal,
}

struct SvgChart {
    extent: Point,
    labels: Vec<SvgLabel>,
    rects: Vec<SvgRect>,
    lines: Vec<SvgLine>,
}

impl Display for SvgChart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"<svg viewBox="0 0 {} {}" xmlns="http://www.w3.org/2000/svg">"#,
            self.extent.x, self.extent.y
        )?;

        for x in &self.rects {
            Display::fmt(&x, f)?;
        }
        for x in &self.lines {
            Display::fmt(&x, f)?;
        }
        for x in &self.labels {
            Display::fmt(&x, f)?;
        }

        f.write_str("</svg>")
    }
}

struct SvgRect {
    start: Point,
    size: Point,
    color: String,
}

impl Display for SvgRect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"<rect x="{}" y="{}" width="{}" height="{}" stroke="{}" />"#,
            self.start.x, self.start.y, self.size.x, self.size.y, self.color
        )
    }
}
pub enum SvgTextAnchor {
    Start,
    Middle,
    End,
}

impl Display for SvgTextAnchor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SvgTextAnchor::Start => f.write_str("start"),
            SvgTextAnchor::Middle => f.write_str("middle"),
            SvgTextAnchor::End => f.write_str("end"),
        }
    }
}
struct SvgLabel {
    a: Point,
    size: Decimal,
    anchor: SvgTextAnchor,
    direction: usize,
    content: String,
}
impl Display for SvgLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"<text x="{}" y="{}" font-size="{}" text-anchor="{}" rotate="{}">{}</text>"#,
            self.a.x, self.a.y, self.size, self.anchor, self.direction, self.content,
        )
    }
}

struct SvgLine {
    a: Point,
    b: Point,
    stroke_width: Decimal,
    dashes: Vec<usize>,
    color: String,
}
impl Display for SvgLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"<line x1="{}" x2="{}" y1="{}" y2="{}" fill="none" stroke="{}" stroke-width="{}" stroke-dasharray="{}"/>"#,
            self.a.x,
            self.b.x,
            self.a.y,
            self.b.y,
            self.color,
            self.stroke_width,
            // TODO, lol.
            self.dashes
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(" "),
        )
    }
}
