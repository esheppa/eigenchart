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

    Ok(())
}
