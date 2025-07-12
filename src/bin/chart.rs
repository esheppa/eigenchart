use std::fs;

use anyhow::Context;
use eigenchart::{Location, RectChart};
use rust_decimal::Decimal;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{EnvFilter, util::SubscriberInitExt};

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .with_ansi(true)
        .with_level(true)
        .with_line_number(true)
        .with_file(true)
        .with_target(true)
        .pretty()
        .finish()
        .init();

    let data = [
        ("Abcasdgasdfg".to_string(), Decimal::new(50, 0)),
        ("defasdgasdfg".to_string(), Decimal::new(30, 0)),
        ("gfhiasdgasdf".to_string(), Decimal::new(70, 0)),
        ("a12asdgasdfg".to_string(), Decimal::new(56, 0)),
        ("a13asdgasdfg".to_string(), Decimal::new(90, 0)),
        ("a14asdgasdfg".to_string(), Decimal::new(85, 0)),
        ("a15asdgasdfg".to_string(), Decimal::new(25, 0)),
        ("a16asdgasdfg".to_string(), Decimal::new(75, 0)),
    ]
    .into();

    // basic horizontal
    // borders when not both

    let basic = RectChart::basic(
        &data,
        Location::Horizontal,
        &[
            Decimal::new(0, 0),
            Decimal::new(20, 0),
            Decimal::new(40, 0),
            Decimal::new(60, 0),
            Decimal::new(80, 0),
            Decimal::new(100, 0),
        ],
        true,
        2,
    );

    let mut svg = basic.render().context("render chart")?;
    svg.flip_y();

    fs::write("chart.svg", svg.to_string()).context("write chart")?;

    // let data = [
    //     (
    //         "abc".to_string(),
    //         WaterfallRect {
    //             start: Decimal::new(20, 0),
    //             end: Decimal::new(50, 0),
    //         },
    //     ),
    //     (
    //         "def".to_string(),
    //         WaterfallRect {
    //             start: Decimal::new(10, 0),
    //             end: Decimal::new(30, 0),
    //         },
    //     ),
    //     (
    //         "ghi".to_string(),
    //         WaterfallRect {
    //             start: Decimal::new(30, 0),
    //             end: Decimal::new(70, 0),
    //         },
    //     ),
    //     (
    //         "a12".to_string(),
    //         WaterfallRect {
    //             start: Decimal::new(16, 0),
    //             end: Decimal::new(56, 0),
    //         },
    //     ),
    //     (
    //         "a13".to_string(),
    //         WaterfallRect {
    //             start: Decimal::new(70, 0),
    //             end: Decimal::new(90, 0),
    //         },
    //     ),
    // ]
    // .into();
    // let basic = RectChart::waterfall(&data, false);

    // let mut svg = basic.render().context("render waterfall")?;
    // svg.flip_y();

    // fs::write("waterfall.svg", svg.to_string()).context("write waterfall")?;

    Ok(())
}
