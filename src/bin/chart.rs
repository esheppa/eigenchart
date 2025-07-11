use std::fs;

use anyhow::Context;
use eigenchart::RectChart;
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
        ("abc".to_string(), Decimal::new(50, 0)),
        ("def".to_string(), Decimal::new(30, 0)),
        ("ghi".to_string(), Decimal::new(70, 0)),
        ("a12".to_string(), Decimal::new(56, 0)),
        ("a13".to_string(), Decimal::new(90, 0)),
    ]
    .into();
    let basic = RectChart::basic_column(&data
    );

    let svg = basic.render().context("render chart")?;

    fs::write("chart.svg", svg.to_string()).context("write chart")?;

    Ok(())
}
