use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
};

use anyhow::{Context, bail, ensure};
use rust_decimal::{Decimal, RoundingStrategy::MidpointAwayFromZero};
use tracing::{error, info};

// mod svg;

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
pub enum Location {
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
pub enum LocationOrdering {
    Before,
    After,
    Both,
}

impl LocationOrdering {
    fn instances(self) -> Decimal {
        match self {
            LocationOrdering::Before => Decimal::ONE,
            LocationOrdering::After => Decimal::ONE,
            LocationOrdering::Both => Decimal::TWO,
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
    data: Vec<(Category<D, O>, [Value<V, E>; N])>,
    // _tooltip: Box<dyn Fn(D, O, V, E) -> String>,
    category_location: Location,
    category_style: Box<dyn Fn(&D, &O) -> RectStyle>,
    display_category: Box<dyn Fn(&D, &O) -> String>,
    plot_category: Box<dyn Fn(&O) -> usize>,
    // tooltip_values: Box<dyn Fn(&V, &E) -> String>,
    plot_values: Box<dyn Fn(&V) -> Decimal>,
    display_value: Box<dyn Fn(&V, &E) -> String>,
    plot_shape: [ShapeInfo; N], // if invalid, eg EndRect before start or multiple start with no end, validation will catch it
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

    categories_border_style: Option<LineStyle>,

    _chart_title: String, // need a better location ... and also specify whether in/out of chart
    width_to_height_ratio: Decimal,
    debug_regions: bool,
}

impl<D, O, V, E, const N: usize> RectChart<D, O, V, E, N> {}

impl RectChart<String, usize, Decimal, (), 2> {
    pub fn basic(
        data: &BTreeMap<String, Decimal>,
        category_location: Location,
        lines: &[Decimal],
        debug_regions: bool,
        decimal_places: u32,
    ) -> Self {
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
                        [
                            Value {
                                value: Decimal::ZERO,
                                extra: (),
                            },
                            Value {
                                value: *v,
                                extra: (),
                            },
                        ],
                    )
                })
                .collect(),
            category_location,
            category_style: Box::new(|_, _| RectStyle {
                color: "red".to_string(),
            }),
            display_category: Box::new(|d, _| d.to_string()),
            plot_category: Box::new(|o| *o),
            display_value: Box::new(move |v, _| {
                v.round_dp_with_strategy(decimal_places, MidpointAwayFromZero)
                    .to_string()
            }),
            plot_values: Box::new(|v| *v),
            plot_shape: [ShapeInfo::StartRect, ShapeInfo::EndRect],
            value_lines: lines
                .iter()
                .map(|d| {
                    (
                        *d,
                        LineStyle {
                            color: "black".to_string(),
                            drawing: LineDrawingStyle::Dashed,
                        },
                    )
                })
                .collect(),
            categories_gutter_proportion: Proportion(Decimal::new(2, 1)),
            categories_name_proportion: Proportion(Decimal::new(22, 2)),
            categoires_name_location: LocationOrdering::Before,
            values_name_proportion: Proportion(Decimal::new(7, 2)),
            values_name_location: LocationOrdering::After,
            categories_border_style: Some(LineStyle {
                color: "black".to_string(),
                drawing: LineDrawingStyle::Solid,
            }),
            _chart_title: "abc".to_string(),
            width_to_height_ratio: Decimal::new(15, 1),
            debug_regions,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct WaterfallRect {
    pub start: Decimal,
    pub end: Decimal,
}

impl RectChart<String, usize, WaterfallRect, (), 2> {
    /// Waterfall chart
    /// - always horizontal...
    pub fn waterfall(data: &BTreeMap<String, WaterfallRect>, debug_regions: bool) -> Self {
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
            category_location: Location::Horizontal,
            category_style: Box::new(|_, _| RectStyle {
                color: "red".to_string(),
            }),
            display_category: Box::new(|d, _| d.to_string()),
            plot_category: Box::new(|o| *o),
            plot_values: Box::new(|v| [v.start, v.end]),
            plot_shape: Box::new(|_, _, _, _| [ShapeInfo::StartRect, ShapeInfo::EndRect]),
            value_lines: Vec::new(),
            categories_gutter_proportion: Proportion(Decimal::new(2, 1)),
            categories_name_proportion: Proportion(Decimal::new(15, 2)),
            categoires_name_location: LocationOrdering::Before,
            values_name_proportion: Proportion(Decimal::new(15, 2)),
            values_name_location: LocationOrdering::Before,
            categories_border_style: None,
            _chart_title: "abc".to_string(),
            width_to_height_ratio: Decimal::ONE,
            debug_regions,
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
    // const ZERO: Proportion = Proportion(Decimal::ZERO);
    // const ONE: Proportion = Proportion(Decimal::ONE);
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
                + self.values_name_proportion.0 * self.values_name_location.instances(),
        )
        .with_context(|| format!("Too much {} proportion", self.category_location))?
        .remainder();

        let values_proportion = Proportion::new(
            self.categories_name_proportion.0 * self.categoires_name_location.instances(),
        )
        .with_context(|| format!("Too much {} proportion", self.category_location.invert()))?
        .remainder();

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
            .filter_map(|(_, v)| (self.plot_values)(&v.value).into_iter().min())
            .min()
        else {
            bail!("Cannot render a chart with empty dataset");
        };

        let Some(max) = self
            .data
            .iter()
            .filter_map(|(_, v)| (self.plot_values)(&v.value).into_iter().max())
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

        let (extent, scaling_factor) = {
            let height = value_plot_range / values_proportion.0;
            let base = Decimal::new(500, 0);
            (
                swap_if_required(Point {
                    x: self.width_to_height_ratio * base,
                    y: base,
                }),
                base / height,
            )
        };

        info!("extent {extent:?}, scaling: {scaling_factor}");

        let mut chart = SvgChart {
            extent,
            labels: Vec::new(),
            rects: Vec::from([]),
            lines: Vec::new(),
        };

        if self.debug_regions {
            chart.rects.push(SvgRect {
                start: Point {
                    x: Decimal::ZERO,
                    y: Decimal::ZERO,
                },
                size: extent,
                color: Some("grey".to_string()),
                stroke: None,
                rounded: None,
            });
        } else {
            // TODO: remove later, useful for now
            chart.rects.push(SvgRect {
                start: Point {
                    x: Decimal::ZERO,
                    y: Decimal::ZERO,
                },
                size: extent,
                color: None,
                stroke: Some(("black".to_string(), 2)),
                rounded: None,
            });
        }

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

        let rect_base_start = match self.categoires_name_location {
            LocationOrdering::Before | LocationOrdering::Both => category_name_width,
            LocationOrdering::After => Decimal::ZERO,
        };

        info!(
            "rect_width={rect_width} gutter_width={gutter_width} value_name_width={value_name_width} category_name_width={category_name_width} rect_area_start={rect_area_start} rect_base_start={rect_base_start}"
        );

        if self.debug_regions {
            if matches!(
                self.categoires_name_location,
                LocationOrdering::Before | LocationOrdering::Both
            ) {
                chart.rects.push(SvgRect {
                    start: swap_if_required(Point {
                        x: if matches!(
                            self.values_name_location,
                            LocationOrdering::Before | LocationOrdering::Both
                        ) {
                            value_name_width
                        } else {
                            Decimal::ZERO
                        },
                        y: Decimal::ZERO,
                    }),
                    size: swap_if_required(Point {
                        x: swap_if_required(chart.extent).x
                            - self.values_name_location.instances() * value_name_width,
                        y: category_name_width,
                    }),
                    color: Some("lightgrey".to_string()),
                    stroke: None,
                    rounded: None,
                });
            }
            if matches!(
                self.categoires_name_location,
                LocationOrdering::After | LocationOrdering::Both
            ) {
                chart.rects.push(SvgRect {
                    start: swap_if_required(Point {
                        x: if matches!(
                            self.values_name_location,
                            LocationOrdering::Before | LocationOrdering::Both
                        ) {
                            value_name_width
                        } else {
                            Decimal::ZERO
                        },
                        y: swap_if_required(chart.extent).y - category_name_width,
                    }),
                    size: swap_if_required(Point {
                        x: swap_if_required(chart.extent).x
                            - self.values_name_location.instances() * value_name_width,
                        y: category_name_width,
                    }),
                    color: Some("lightgrey".to_string()),
                    stroke: None,
                    rounded: None,
                });
            }

            // ad values name area(s)

            if matches!(
                self.values_name_location,
                LocationOrdering::Before | LocationOrdering::Both
            ) {
                chart.rects.push(SvgRect {
                    start: swap_if_required(Point {
                        x: Decimal::ZERO,
                        y: if matches!(
                            self.categoires_name_location,
                            LocationOrdering::Before | LocationOrdering::Both
                        ) {
                            category_name_width
                        } else {
                            Decimal::ZERO
                        },
                    }),
                    size: swap_if_required(Point {
                        x: value_name_width,
                        y: swap_if_required(chart.extent).y
                            - self.categoires_name_location.instances() * category_name_width,
                    }),
                    color: Some("lightblue".to_string()),
                    stroke: None,
                    rounded: None,
                });
            }
            if matches!(
                self.values_name_location,
                LocationOrdering::After | LocationOrdering::Both
            ) {
                chart.rects.push(SvgRect {
                    start: swap_if_required(Point {
                        x: swap_if_required(chart.extent).x - value_name_width,
                        y: if matches!(
                            self.categoires_name_location,
                            LocationOrdering::Before | LocationOrdering::Both
                        ) {
                            category_name_width
                        } else {
                            Decimal::ZERO
                        },
                    }),
                    size: swap_if_required(Point {
                        x: value_name_width,
                        y: swap_if_required(chart.extent).y
                            - self.categoires_name_location.instances() * category_name_width,
                    }),
                    color: Some("lightblue".to_string()),
                    stroke: None,
                    rounded: None,
                });
            }
        }

        for (plot_idx, (_plot_category, (c, v))) in map.into_iter().enumerate() {
            let category_style = (self.category_style)(&c.display, &c.ordering);
            let display_category = (self.display_category)(&c.display, &c.ordering);
            let plot_values = (self.plot_values)(&v.value);
            let plot_shape = (self.plot_shape)(&c.display, &c.ordering, &v.value, &v.extra);

            // calculate rect width start/end
            let rect_width_start =
                rect_area_start + Decimal::from(plot_idx) * (rect_width + gutter_width);
            let rect_width_end = rect_width_start + rect_width;

            info!(
                "category_style={category_style:?} display_category={display_category} plot_values={plot_values:?} plot_shape={plot_shape:?} rect_width_start={rect_width_start} rect_width_end={rect_width_end}"
            );

            let mut started_rect = None;
            for i in 0..N {
                let previous_shape = i.checked_sub(1).map(|ix| plot_shape[ix]);
                let _previous_plot_value = i
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
                let plot_value = plot_values[i] - value_plot_min;

                if matches!(
                    self.categoires_name_location,
                    LocationOrdering::Before | LocationOrdering::Both
                ) {
                    chart.labels.push(SvgLabel {
                        a: swap_if_required(Point {
                            x: rect_width_start + rect_width / Decimal::TWO,
                            y: (category_name_width - Decimal::new(12, 0)) / Decimal::TWO,
                        }),
                        size: Decimal::new(12, 0),
                        anchor: SvgTextAnchor::Middle,
                        direction: 0,
                        content: display_category.to_string(),
                        text_length: Some(rect_width),
                    });
                }

                if matches!(
                    self.categoires_name_location,
                    LocationOrdering::After | LocationOrdering::Both
                ) {
                    chart.labels.push(SvgLabel {
                        a: swap_if_required(Point {
                            x: rect_width_start + rect_width / Decimal::TWO,
                            y: swap_if_required(chart.extent).y
                                - (category_name_width + Decimal::new(12, 0)) / Decimal::TWO,
                        }),
                        size: Decimal::new(12, 0),
                        anchor: SvgTextAnchor::Middle,
                        direction: 0,
                        content: display_category.to_string(),
                        text_length: None,
                    });
                }

                match (plot_shape[i], previous_shape, started_rect) {
                    (ShapeInfo::Line, _, _) => {
                        // for a line we just plot it whatever
                        let line_a = swap_if_required(Point {
                            x: rect_width_start * self.width_to_height_ratio,
                            y: plot_value * scaling_factor,
                        });
                        let line_b = swap_if_required(Point {
                            x: rect_width_end * self.width_to_height_ratio,
                            y: plot_value * scaling_factor,
                        });
                        info!("Pushing line {line_a} to {line_b}");
                        chart.lines.push(SvgLine {
                            a: line_a,
                            b: line_b,
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
                        started_rect = Some(plot_value);
                    }
                    (ShapeInfo::EndRect, _, Some(start)) if start > plot_value => {
                        bail!("Invalid end-rect before started")
                    }
                    (ShapeInfo::EndRect, _, Some(start_value)) => {
                        let start = swap_if_required(Point {
                            x: rect_width_start, // * self.width_to_height_ratio,
                            y: rect_base_start + start_value * scaling_factor,
                        });
                        let size = swap_if_required(Point {
                            x: rect_width, // * self.width_to_height_ratio,
                            y: (plot_value - start_value) * scaling_factor,
                        });
                        info!("Pushing rect: corner {start}, size {size}");
                        chart.rects.push(SvgRect {
                            start,
                            size,

                            color: Some(category_style.color.to_string()),
                            stroke: None,
                            rounded: Some(2),
                        })
                    }
                    (a, b, c) => {
                        bail!("Unexpected combination {a:?} and {b:?} and started rect {c:?}");
                    }
                };
            }
        }

        if let Some(s) = &self.categories_border_style {
            chart.lines.push(SvgLine {
                a: swap_if_required(Point {
                    x: match self.values_name_location {
                        LocationOrdering::Before | LocationOrdering::Both => value_name_width,
                        LocationOrdering::After => Decimal::ZERO,
                    },
                    y: scaling_factor * value_plot_min
                        + match self.categoires_name_location {
                            LocationOrdering::Before | LocationOrdering::Both => {
                                category_name_width
                            }
                            LocationOrdering::After => Decimal::ZERO,
                        },
                }),
                b: swap_if_required(Point {
                    x: match self.values_name_location {
                        LocationOrdering::After | LocationOrdering::Both => value_name_width,
                        LocationOrdering::Before => Decimal::ZERO,
                    },
                    y: scaling_factor * value_plot_max
                        + match self.categoires_name_location {
                            LocationOrdering::Before | LocationOrdering::Both => {
                                category_name_width
                            }
                            LocationOrdering::After => Decimal::ZERO,
                        },
                }),
                stroke_width: Decimal::ONE,
                dashes: match s.drawing {
                    LineDrawingStyle::Solid => Vec::new(),
                    LineDrawingStyle::Dashed => Vec::from([3]),
                    LineDrawingStyle::Dotted => Vec::from([2, 8]),
                    LineDrawingStyle::Custom(x) => x.into(),
                },
                color: s.color.to_string(),
            });
            chart.lines.push(SvgLine {
                a: swap_if_required(Point {
                    x: swap_if_required(chart.extent).x
                        - match self.values_name_location {
                            LocationOrdering::Before | LocationOrdering::Both => value_name_width,
                            LocationOrdering::After => Decimal::ZERO,
                        },
                    y: scaling_factor * value_plot_min
                        + match self.categoires_name_location {
                            LocationOrdering::Before | LocationOrdering::Both => {
                                category_name_width
                            }
                            LocationOrdering::After => Decimal::ZERO,
                        },
                }),
                b: swap_if_required(Point {
                    x: swap_if_required(chart.extent).x
                        - match self.values_name_location {
                            LocationOrdering::After | LocationOrdering::Both => value_name_width,
                            LocationOrdering::Before => Decimal::ZERO,
                        },
                    y: scaling_factor * value_plot_max
                        + match self.categoires_name_location {
                            LocationOrdering::Before | LocationOrdering::Both => {
                                category_name_width
                            }
                            LocationOrdering::After => Decimal::ZERO,
                        },
                }),
                stroke_width: Decimal::ONE,
                dashes: match s.drawing {
                    LineDrawingStyle::Solid => Vec::new(),
                    LineDrawingStyle::Dashed => Vec::from([3]),
                    LineDrawingStyle::Dotted => Vec::from([2, 8]),
                    LineDrawingStyle::Custom(x) => x.into(),
                },
                color: s.color.to_string(),
            });
        }

        // add value lines and labels
        // TODO: text wrapping. just render on one line for now
        for (v, s) in &self.value_lines {
            // can render in any random order

            // make sure to chuck the text in the right place

            // full width of the chart area
            // starts from the rect start
            chart.lines.push(SvgLine {
                a: swap_if_required(Point {
                    x: match self.values_name_location {
                        LocationOrdering::Before | LocationOrdering::Both => value_name_width,
                        LocationOrdering::After => Decimal::ZERO,
                    },
                    y: scaling_factor * (v - value_plot_min)
                        + match self.categoires_name_location {
                            LocationOrdering::Before | LocationOrdering::Both => {
                                category_name_width
                            }
                            LocationOrdering::After => Decimal::ZERO,
                        },
                }),
                b: swap_if_required(Point {
                    x: swap_if_required(chart.extent).x
                        - match self.values_name_location {
                            LocationOrdering::After | LocationOrdering::Both => value_name_width,
                            LocationOrdering::Before => Decimal::ZERO,
                        },
                    y: scaling_factor * (v - value_plot_min)
                        + match self.categoires_name_location {
                            LocationOrdering::Before | LocationOrdering::Both => {
                                category_name_width
                            }
                            LocationOrdering::After => Decimal::ZERO,
                        },
                }),
                stroke_width: Decimal::ONE,
                dashes: match s.drawing {
                    LineDrawingStyle::Solid => Vec::new(),
                    LineDrawingStyle::Dashed => Vec::from([3]),
                    LineDrawingStyle::Dotted => Vec::from([2, 8]),
                    LineDrawingStyle::Custom(x) => x.into(),
                },
                color: s.color.to_string(),
            });

            if matches!(
                self.values_name_location,
                LocationOrdering::Before | LocationOrdering::Both
            ) {}

            if matches!(
                self.values_name_location,
                LocationOrdering::After | LocationOrdering::Both
            ) {}
        }

        Ok(chart)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ShapeInfo {
    Line,
    BeforeConnectedLine,
    AfterConnectedLine,
    StartRect,
    EndRect,
}

// struct TextStyle {
//     color: String,
//     // later font, etc
//     // size? proportional?
// }

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

pub enum LineDrawingStyle {
    Solid,
    Dashed,
    Dotted,
    Custom([usize; 2]),
}

// another chart which is like a scatter will need to be more efficient
// struct PointChart<P, O, I, N, T>

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Point {
    x: Decimal,
    y: Decimal,
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
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

impl SvgChart {
    pub fn flip_y(&mut self) {
        for label in &mut self.labels {
            label.flip_y(self.extent);
        }
        for rect in &mut self.rects {
            rect.flip_y(self.extent);
        }
        for line in &mut self.lines {
            line.flip_y(self.extent);
        }
    }

    pub fn swap(&mut self) {
        for label in &mut self.labels {
            label.swap();
        }
        for rect in &mut self.rects {
            rect.swap();
        }
        for line in &mut self.lines {
            line.swap();
        }
    }
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
    color: Option<String>,
    stroke: Option<(String, usize)>,
    rounded: Option<usize>,
}

impl SvgRect {
    fn swap(&mut self) {
        self.start = self.start.swap();
        self.size = self.size.swap();
    }
    fn flip_y(&mut self, extent: Point) {
        self.start = Point {
            y: extent.y - self.start.y - self.size.y,
            ..self.start
        };
    }
}

impl Display for SvgRect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"<rect x="{}" y="{}" width="{}" height="{}" "#,
            self.start.x, self.start.y, self.size.x, self.size.y,
        )?;

        if let Some(c) = &self.color {
            write!(f, r#"fill="{c}" "#)?;
        } else {
            write!(f, r#"fill="none" "#)?;
        }
        if let Some((sc, sw)) = &self.stroke {
            write!(f, r#"stroke-width="{sw}" stroke="{sc}" "#)?;
        }

        if let Some(rad) = &self.rounded {
            write!(f, r#"rx="{rad}" ry="{rad}" "#)?;
        }

        f.write_str(r#"/>"#)
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
    text_length: Option<Decimal>,
}

impl SvgLabel {
    fn swap(&mut self) {
        self.a = self.a.swap();
    }
    fn flip_y(&mut self, extent: Point) {
        self.a = Point {
            y: extent.y - self.a.y,
            ..self.a
        };
    }
}
impl Display for SvgLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"<text x="{}" y="{}" font-size="{}" text-anchor="{}" rotate="{}" "#,
            self.a.x, self.a.y, self.size, self.anchor, self.direction,
        )?;

        if let Some(l) = self.text_length {
            write!(f, r#"textLength="{l}" "#,)?;
        }

        write!(f, r#">{}</text>"#, self.content,)
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

impl SvgLine {
    fn swap(&mut self) {
        self.a = self.a.swap();
        self.b = self.b.swap();
    }
    fn flip_y(&mut self, extent: Point) {
        self.a = Point {
            y: extent.y - self.a.y,
            ..self.a
        };
        self.b = Point {
            y: extent.y - self.b.y,
            ..self.b
        }
    }
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
