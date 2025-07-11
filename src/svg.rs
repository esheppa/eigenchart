use std::{
    fmt::Display,
    iter::Sum,
    num::NonZeroU32,
    ops::{Add, AddAssign, Mul},
};

use serde::{Deserialize, Serialize};
use tracing::info;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonZeroMm(NonZeroU32);

impl NonZeroMm {
    pub fn new(mm: Mm) -> Option<NonZeroMm> {
        NonZeroU32::new(mm.0).map(NonZeroMm)
    }
    pub fn get(self) -> NonZeroU32 {
        self.0
    }
    pub fn mm(self) -> Mm {
        Mm(self.0.get())
    }
    pub fn checked_add(&self, rhs: NonZeroMm) -> Option<NonZeroMm> {
        self.0.checked_add(rhs.0.get()).map(NonZeroMm)
    }

    pub fn checked_mul(&self, oth: NonZeroU32) -> Option<NonZeroMm> {
        self.0.checked_mul(oth).map(NonZeroMm)
    }
}

impl Display for NonZeroMm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} mm", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SvgPoint {
    pub x: Mm,
    pub y: Mm,
}

impl AddAssign for SvgPoint {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Add for SvgPoint {
    type Output = SvgPoint;

    fn add(self, rhs: Self) -> Self::Output {
        SvgPoint {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Display for SvgPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}

impl SvgPoint {
    pub fn origin() -> SvgPoint {
        SvgPoint { x: Mm(0), y: Mm(0) }
    }
    // pub fn make_vec_to(self, other: SvgPoint) -> Option<RectilinearVector> {
    //     let direction = if self.x == other.x && self.y > other.y {
    //         SvgVecDirection::North(self.y.checked_sub(other.y).and_then(NonZeroMm::new)?)
    //     } else if self.x == other.x && self.y < other.y {
    //         SvgVecDirection::South(other.y.checked_sub(self.y).and_then(NonZeroMm::new)?)
    //     } else if self.y == other.y && self.x > other.x {
    //         SvgVecDirection::West(self.x.checked_sub(other.x).and_then(NonZeroMm::new)?)
    //     } else if self.y == other.y && self.x < other.x {
    //         SvgVecDirection::East(other.x.checked_sub(self.x).and_then(NonZeroMm::new)?)
    //     } else {
    //         return None;
    //     };

    //     // verify vec is valid!
    //     self.add_direction(direction)?;

    //     Some(RectilinearVector {
    //         start: self,
    //         direction,
    //     })
    // }
    pub fn add_direction(self, direction: SvgVecDirection) -> Option<SvgPoint> {
        match direction {
            SvgVecDirection::East(x) => SvgPoint {
                x: self.x.checked_add(x.mm())?,
                ..self
            },
            SvgVecDirection::West(x) => SvgPoint {
                x: self.x.checked_sub(x.mm())?,
                ..self
            },
            SvgVecDirection::South(y) => SvgPoint {
                y: self.y.checked_add(y.mm())?,
                ..self
            },
            SvgVecDirection::North(y) => SvgPoint {
                y: self.y.checked_sub(y.mm())?,
                ..self
            },
        }
        .into()
    }
}

#[derive(
    Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default,
)]
#[serde(transparent)]
pub struct Mm(u32);

impl Sum for Mm {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut start = Mm::default();
        for i in iter {
            start = start + i;
        }

        start
    }
}
impl Display for Mm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} mm", self.0)
    }
}
impl AddAssign for Mm {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Add for Mm {
    type Output = Mm;

    fn add(self, rhs: Self) -> Self::Output {
        Mm(self.0 + rhs.0)
    }
}

impl Mul<Mm> for u32 {
    type Output = Mm;

    fn mul(self, rhs: Mm) -> Self::Output {
        Mm(self * rhs.0)
    }
}
impl Mul<u32> for Mm {
    type Output = Mm;

    fn mul(self, rhs: u32) -> Self::Output {
        Mm(self.0 * rhs)
    }
}
impl Mm {
    pub fn new(mm: u32) -> Mm {
        Mm(mm)
    }
    pub fn get(self) -> u32 {
        self.0
    }
    pub fn checked_add(&self, rhs: Mm) -> Option<Mm> {
        self.0.checked_add(rhs.0).map(Mm)
    }
    pub fn checked_sub(&self, rhs: Mm) -> Option<Mm> {
        self.0.checked_sub(rhs.0).map(Mm)
    }
    pub fn checked_mul(&self, oth: u32) -> Option<Mm> {
        self.0.checked_mul(oth).map(Mm)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SvgVecDirection {
    East(NonZeroMm),
    West(NonZeroMm),
    South(NonZeroMm),
    North(NonZeroMm),
}

impl SvgVecDirection {
    fn length(self) -> NonZeroMm {
        match self {
            SvgVecDirection::East(x) => x,
            SvgVecDirection::West(x) => x,
            SvgVecDirection::South(x) => x,
            SvgVecDirection::North(x) => x,
        }
    }
    fn switch(self) -> SvgVecDirection {
        match self {
            SvgVecDirection::East(x) => SvgVecDirection::West(x),
            SvgVecDirection::West(x) => SvgVecDirection::East(x),
            SvgVecDirection::South(x) => SvgVecDirection::North(x),
            SvgVecDirection::North(x) => SvgVecDirection::South(x),
        }
    }
    fn matches_direction(self, other: SvgVecDirection) -> bool {
        match (self, other) {
            (SvgVecDirection::East(_), SvgVecDirection::East(_))
            | (SvgVecDirection::West(_), SvgVecDirection::West(_))
            | (SvgVecDirection::South(_), SvgVecDirection::South(_))
            | (SvgVecDirection::North(_), SvgVecDirection::North(_)) => true,
            _ => false,
        }
    }
}

// (0,0) is top left
pub struct Svg {
    border_width: Mm,
    extent: SvgPoint,
    elements: Vec<SvgElement>,
}

impl Svg {
    pub fn new(border_width: Mm) -> Svg {
        Svg {
            border_width,
            extent: SvgPoint::origin(),
            elements: Vec::new(),
        }
    }

    pub fn rotate(&mut self, n: u8) {
        for _ in 0..n {
            self.rotate_clockwise();
        }
    }

    fn rotate_clockwise(&mut self) {
        self.extent = SvgPoint {
            x: self.extent.y,
            y: self.extent.x,
        };

        let rot = |p: SvgPoint| SvgPoint {
            x: self.extent.x.checked_sub(p.y).unwrap(),
            y: p.x,
        };

        for el in &mut self.elements {
            match el {
                SvgElement::Line { start, end, .. } => {
                    *start = rot(*start);
                    *end = rot(*end);
                }
                SvgElement::Text { point, .. } => {
                    *point = rot(*point);
                }
            }
        }
    }

    pub fn rescale(&mut self) {
        // find min element

        let min = self
            .elements
            .iter()
            .map(|e| e.min())
            .fold(self.extent, |mut acc, x| {
                // info!("Min for {x:?}");
                acc.x = acc.x.min(x.x);
                acc.y = acc.y.min(x.y);
                acc
            });

        // info!("rescale from {min}");
        let Some(x) = min.x.checked_sub(self.border_width) else {
            info!("Exit: {} smaller than {}", min.x, self.border_width);
            return;
        };

        let Some(y) = min.y.checked_sub(self.border_width) else {
            info!("Exit: {} smaller than {}", min.y, self.border_width);

            return;
        };

        // info!("Min: {min}, {x}, {y}");

        for e in &mut self.elements {
            e.offset_sub(SvgPoint { x, y });
        }

        let max = self
            .elements
            .iter()
            .map(|e| e.max())
            .fold(SvgPoint::origin(), |mut acc, x| {
                acc.x = acc.x.max(x.x);
                acc.y = acc.y.max(x.y);
                acc
            });

        // info!("Max: {max}, old extent: {}", self.extent);

        self.extent.x = max.x + self.border_width;
        self.extent.y = max.y + self.border_width;
        // info!("Max: {max}, new extent: {}", self.extent);
    }
    pub fn push_element(&mut self, mut element: SvgElement) {
        match element {
            SvgElement::Line { start, end, .. } => {
                // update the dimensions

                self.extent.x = self
                    .extent
                    .x
                    .max(start.x.max(end.x) + self.border_width.checked_mul(2).unwrap());
                self.extent.y = self
                    .extent
                    .y
                    .max(start.y.max(end.y) + self.border_width.checked_mul(2).unwrap());
            }
            SvgElement::Text { .. } => {
                // do nothing
            }
        }

        element.offset_add(SvgPoint {
            x: self.border_width,
            y: self.border_width,
        });
        self.elements.push(element);
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

pub enum SvgElement {
    Line {
        start: SvgPoint,
        end: SvgPoint,
        stroke: String,
        stroke_width: u32,
    },
    Text {
        point: SvgPoint,
        content: String,
        direction: SvgVecDirection,
        anchor: SvgTextAnchor,
        size: u32,
    },
}
impl SvgElement {
    fn min(&self) -> SvgPoint {
        match self {
            SvgElement::Line { start, end, .. } => SvgPoint {
                x: start.x.min(end.x),
                y: start.y.min(end.y),
            },
            SvgElement::Text { point, .. } => *point,
        }
    }

    fn max(&self) -> SvgPoint {
        match self {
            SvgElement::Line { start, end, .. } => SvgPoint {
                x: start.x.max(end.x),
                y: start.y.max(end.y),
            },
            SvgElement::Text { point, .. } => *point,
        }
    }
    fn offset_add(&mut self, offset: SvgPoint) {
        match self {
            SvgElement::Line { start, end, .. } => {
                *start += offset;
                *end += offset;
            }
            SvgElement::Text { point, .. } => {
                *point += offset;
            }
        }
    }
    fn offset_sub(&mut self, offset: SvgPoint) {
        match self {
            SvgElement::Line { start, end, .. } => {
                start.x = start.x.checked_sub(offset.x).unwrap_or_default();
                start.y = start.y.checked_sub(offset.y).unwrap_or_default();
                end.x = end.x.checked_sub(offset.x).unwrap_or_default();
                end.y = end.y.checked_sub(offset.y).unwrap_or_default();
            }
            SvgElement::Text { point, .. } => {
                point.x = point.x.checked_sub(offset.x).unwrap_or_default();
                point.y = point.y.checked_sub(offset.y).unwrap_or_default();
            }
        }
    }
}

impl Display for Svg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            r#"<svg viewBox="0 0 {} {}" xmlns="http://www.w3.org/2000/svg">"#,
            self.extent.x.get(),
            self.extent.y.get()
        )?;

        for el in &self.elements {
            match el {
                SvgElement::Line {
                    start,
                    end,
                    stroke,
                    stroke_width,
                } => {
                    write!(
                        f,
                        r#"<line x1="{}" x2="{}" y1="{}" y2="{}" fill="none" stroke="{stroke}" stroke-width="{stroke_width}"/>"#,
                        start.x.get(),
                        end.x.get(),
                        start.y.get(),
                        end.y.get()
                    )?;
                }
                SvgElement::Text {
                    point: SvgPoint { x, y },
                    anchor,
                    content,
                    direction,
                    size,
                } => {
                    write!(
                        f,
                        r#"<text x="{}" y="{}" font-size="{size}" text-anchor="{anchor}" rotate="{}">{content}</text>"#,
                        x.get(),
                        y.get(),
                        match direction {
                            SvgVecDirection::East(_) => 0,
                            SvgVecDirection::West(_) => 180,
                            SvgVecDirection::South(_) => 90,
                            SvgVecDirection::North(_) => 270,
                        }
                    )?;
                }
            }
        }

        f.write_str("</svg>")
    }
}
