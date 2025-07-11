use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use anyhow::{Context, bail, ensure};
use rust_decimal::Decimal;
use tracing::{Level, error, event, info};

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
pub struct RectChart<D, O, V, E, const N: usize> {
    data: Vec<(Category<D, O>, Value<V, E>)>,
    tooltip: Box<dyn Fn(D, O, V, E) -> String>,
    category_location: Location,
    category_style: Box<dyn Fn(&D, &O) -> RectStyle>,
    display_category: Box<dyn Fn(&D, &O) -> String>,
    plot_category: Box<dyn Fn(&O) -> usize>,
    tooltip_values: Box<dyn Fn(&V) -> [String; N]>,
    plot_vaues: Box<dyn Fn(&V) -> [Decimal; N]>,
    plot_shape: Box<dyn Fn(&D, &O, &V, &E) -> [ShapeInfo; N]>, // if invalid, eg EndRect before start or multiple start with no end, validation will catch it
    // these don't need to be calculated dynamically as we already have all the data by now
    // don't need lines to seperate categories...
    value_lines: Vec<(Decimal, LineStyle)>,

    // where the proportion is for a dynamic number of things, it applies once
    categories_gutter_proportion: Proportion,
    // categories_margin_proportion: Proportion,
    // categories_padding_proportion: Proportion,
    categories_name_proportion: Proportion,
    categoires_name_location: LocationOrdering,

    // where it is for a known limited set of things it applies each time, here it is applied once or twice (or zero times????)
    values_name_proportion: Proportion,

    values_name_location: LocationOrdering,

    horizontal_border_style: Option<LineStyle>,
    vertical_border_style: Option<LineStyle>,
    value_line_style: LineStyle,

    chart_title: String, // need a better location ... and also specify whether in/out of chart
    width_to_height_ratio: Decimal,
}

impl<D, O, V, E, const N: usize> RectChart<D, O, V, E, N> {}

impl RectChart<String, usize, Decimal, (), 2> {
    pub fn basic_column(data: &BTreeMap<String, Decimal>) -> Self {
        RectChart {
            data: data
                .iter()
                .enumerate()
                .map(|(ordering, (c, v))| {
                    (
                        Category {
                            display: c.to_string(),
                            ordering,
                        },
                        Value {
                            value: *v,
                            extra: (),
                        },
                    )
                })
                .collect(),
            tooltip: Box::new(|_, _, _, _| String::new()),
            category_location: Location::Horizontal,
            category_style: Box::new(|_, _| RectStyle {
                color: "red".to_string(),
            }),
            display_category: Box::new(|d, _| d.to_string()),
            plot_category: Box::new(|o| *o),
            tooltip_values: Box::new(|_| [String::new(), String::new()]),
            plot_vaues: Box::new(|v| [Decimal::ZERO, *v]),
            plot_shape: Box::new(|_, _, _, _| [ShapeInfo::StartRect, ShapeInfo::EndRect]),
            value_lines: Vec::new(),
            categories_gutter_proportion: Proportion(Decimal::new(1, 1)),
            categories_name_proportion: Proportion(Decimal::new(2, 1)),
            categoires_name_location: LocationOrdering::Before,
            values_name_proportion: Proportion(Decimal::new(2, 1)),
            values_name_location: LocationOrdering::Before,
            horizontal_border_style: None,
            vertical_border_style: None,
            value_line_style: LineStyle {
                color: "black".to_string(),
                drawing: LineDrawingStyle::Solid,
            },
            chart_title: "abc".to_string(),
            width_to_height_ratio: Decimal::ONE,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
struct Proportion(Decimal);

impl Display for Proportion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
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
    pub fn render(&self) -> anyhow::Result<SvgChart> {
        ensure!(
            self.width_to_height_ratio > Decimal::ZERO,
            "Cannot render a chart with width less than or equal to * the height",
        );
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

        info!(
            "categories_proportion={categories_proportion},values_proportion={values_proportion},category_set={}..{},min={min},max={max}",
            category_set.first().unwrap(),
            category_set.last().unwrap()
        );

        // this will be used for queries proportional to the values
        let values_range = max - min;

        let lines_min = self.value_lines.iter().map(|(v, _)| v).min().copied();
        let lines_max = self.value_lines.iter().map(|(v, _)| v).max().copied();

        let value_plot_min = lines_min.unwrap_or(min).min(min);
        let value_plot_max = lines_max.unwrap_or(max).max(max);

        let value_plot_range = value_plot_max - value_plot_min;
        // figure out the total size we need

        info!(
            "values_range={values_range} lines_min={lines_min:?} lines_max={lines_max:?} value_plot_min={value_plot_min} value_plot_max={value_plot_max} value_plot_range={value_plot_range}"
        );

        let swap_if_required = |p: Point| match self.category_location {
            Location::Horizontal => p,
            Location::Vertical => p.swap(),
        };

        let extent = {
            let height = value_plot_range / values_proportion.remainder().0;

            swap_if_required(Point {
                x: self.width_to_height_ratio * height,
                y: height,
            })
        };

        info!("extent {extent:?}");

        let mut chart = SvgChart {
            extent,
            labels: Vec::new(),
            rects: Vec::from([
                SvgRect {
                    start: Point { x: Decimal::ZERO, y: Decimal::ZERO},
                    size: extent,
                    color: "grey".to_string(),
                }
            ]),
            lines: Vec::new(),
        };
        

        // add all rects and labels
        // TODO: text wrapping. just render on one line for now
        // we have alreay ensured that there are no overlapping
        // organize into ordered plot categories
        // enumerate it
        // so that we know how to plot

        let map = self
            .data
            .iter()
            .map(|x| ((self.plot_category)(&x.0.ordering), x))
            .collect::<BTreeMap<_, _>>();

        let (rect_width, gutter_width, value_name_width, category_name_width) = match self
            .category_location
        {
            Location::Horizontal => (
                chart.extent.x * categories_proportion.0 / Decimal::from(map.len()),
                chart.extent.x * self.categories_gutter_proportion.0 / Decimal::from(map.len() + 1),
                chart.extent.x * self.values_name_proportion.0,
                chart.extent.y * self.categories_name_proportion.0,
            ),
            Location::Vertical => (
                chart.extent.y * categories_proportion.0 / Decimal::from(map.len()),
                chart.extent.y * self.categories_gutter_proportion.0 / Decimal::from(map.len() + 1),
                chart.extent.y * self.values_name_proportion.0,
                chart.extent.x * self.categories_name_proportion.0,
            ),
        };

        let rect_area_start = match self.values_name_location {
            // will be auto applied at the end due to the rects and gutters being scaled down by the required space for the names
            LocationOrdering::Before | LocationOrdering::Both => value_name_width,
            LocationOrdering::After => Decimal::ZERO,
        } + gutter_width; // add one gutter, will add one more after each rect is added
        // let rect_area_end = match self.values_name_location {
        //     LocationOrdering::Before => todo!(),
        //     LocationOrdering::After => todo!(),
        //     LocationOrdering::Both => todo!(),
        // };

        let rect_base_start = match self.categoires_name_location {
            LocationOrdering::Before | LocationOrdering::Both => category_name_width,
            LocationOrdering::After => Decimal::ZERO,
        };

        for (plot_idx, (plot_category, (c, v))) in map.into_iter().enumerate() {
            let category_style = (self.category_style)(&c.display, &c.ordering);
            let display_category = (self.display_category)(&c.display, &c.ordering);
            let tooltip_values = (self.tooltip_values)(&v.value);
            let plot_values = (self.plot_vaues)(&v.value);
            let plot_shape = (self.plot_shape)(&c.display, &c.ordering, &v.value, &v.extra);

            // calculate rect width start/end
            let rect_width_start = rect_area_start + Decimal::from(plot_idx) * (rect_width + gutter_width);
            let rect_width_end = rect_width_start + rect_width;

            info!(
                "category_style={category_style:?} display_category={display_category} tooltip_values={tooltip_values:?} plot_values={plot_values:?} plot_shape={plot_shape:?} rect_width_start={rect_width_start} rect_width_end={rect_width_end}"
            );
            let mut started_rect = None;
            for i in 0..N {
                let previous_shape = i.checked_sub(1).map(|ix| plot_shape[ix]);
                let previous_plot_value = i
                    .checked_sub(1)
                    .map(|ix| plot_values[ix])
                    .unwrap_or_default()
                    + rect_base_start;

                if i == N - 1
                    && matches!(
                        plot_shape[i],
                        ShapeInfo::StartRect | ShapeInfo::AfterConnectedLine
                    )
                {
                    bail!("nah");
                }
                match (plot_shape[i], previous_shape, started_rect) {
                    (ShapeInfo::Line, _, _) => {
                        // for a line we just plot it whatever
                        chart.lines.push(SvgLine {
                            a: swap_if_required(Point {
                                x: rect_width_start,
                                y: plot_values[i],
                            }),
                            b: swap_if_required(Point {
                                x: rect_width_end,
                                y: plot_values[i],
                            }),
                            stroke_width: Decimal::ONE,
                            dashes: Vec::new(),
                            color: category_style.color.to_string(),
                        });
                    }
                    (ShapeInfo::BeforeConnectedLine, Some(_), _) => {
                        error!("TODO: BeforeConnectedLine");
                    }
                    (ShapeInfo::AfterConnectedLine, None, _) => {
                        error!("TODO: AfterConnectedLine");
                    }
                    (ShapeInfo::AfterConnectedLine, Some(_), _) => {
                        error!("TODO: AfterConnectedLine");
                    }
                    (ShapeInfo::StartRect, _, None) => {
                        // nothing to do, added when we reach the end rect
                        started_rect = Some(plot_values[i]);
                    }
                    (ShapeInfo::EndRect, _, Some(start)) if start > plot_values[i] => {
                        bail!("Invalid end-rect before started")
                    }
                    (ShapeInfo::EndRect, _, Some(start)) => chart.rects.push(SvgRect {
                        start: swap_if_required(Point {
                            x: rect_width_start,
                            y: start,
                        }),
                        size: swap_if_required(Point {
                            x: rect_width,
                            y: plot_values[i] - start,
                        }),
                        color: category_style.color.to_string(),
                    }),
                    (a, b, c) => {
                        bail!("Unexpected combination {a:?} and {b:?} and started rect {c:?}");
                    }
                };
            }
        }

        // add value lines and labels
        // TODO: text wrapping. just render on one line for now
        for (v, s) in &self.value_lines {
            // can render in any random order
            // make sure to chuck the text in the right place

            // TODO:  later...
        }

        Ok(chart)
    }
}

#[derive(Debug, Clone, Copy)]
enum ShapeInfo {
    Line,
    BeforeConnectedLine,
    AfterConnectedLine,
    StartRect,
    EndRect,
}

struct TextStyle {
    color: String,
    // later font, etc
    // size? proportional?
}

#[derive(Debug)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Point {
    x: Decimal,
    y: Decimal,
}

impl Point {
    fn swap(self) -> Point {
        Point {
            x: self.y,
            y: self.x,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SvgChart {
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
#[derive(Debug, Clone)]

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
#[derive(Debug, Clone)]

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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
