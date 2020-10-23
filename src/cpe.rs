use language_tags::LanguageTag;

use crate::component::Component;
use crate::error::{CpeError, Result};
use std::convert::TryFrom;
use std::str::FromStr;

/// CPE Language value
///
/// May be "ANY", or a valid RFC-5646 language tag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Language {
    Any,
    Language(LanguageTag),
}

impl Default for Language {
    fn default() -> Self {
        Language::Any
    }
}

impl FromStr for Language {
    type Err = CpeError;
    fn from_str(s: &str) -> Result<Self> {
        if s == "ANY" {
            Ok(Self::Any)
        } else {
            Ok(Self::Language(s.parse()?))
        }
    }
}

#[cfg(feature = "display")]
use std::fmt;
#[cfg(feature = "display")]
impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Any => if f.alternate() {
                write!(f, "")
            } else {
                write!(f, "ANY")
            },
            Self::Language(tag) => write!(f, "{}", tag),
        }
    }
}

pub trait Cpe {
    fn part(&self) -> Option<CpeType>;
    fn vendor(&self) -> &Component;
    fn product(&self) -> &Component;
    fn version(&self) -> &Component;
    fn update(&self) -> &Component;
    fn edition(&self) -> &Component;
    fn language(&self) -> &Language;
    fn sw_edition(&self) -> &Component;
    fn target_sw(&self) -> &Component;
    fn target_hw(&self) -> &Component;
    fn other(&self) -> &Component;
}

/// A CPE Type Component
///
/// One of
/// * h - hardware
/// * o - operating system
/// * a - application
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CpeType {
    Any,
    Hardware,
    OperatingSystem,
    Application,
}

impl Default for CpeType {
    fn default() -> Self {
        Self::Any
    }
}

impl TryFrom<&str> for CpeType {
    type Error = CpeError;

    fn try_from(val: &str) -> Result<Self> {
        if val == "ANY" {
            return Ok(Self::Any);
        }
        let c = {
            let c = val.chars().next();

            //TODO error + check for next None

            c.unwrap()
        };
        match c {
            'h' => Ok(Self::Hardware),
            'o' => Ok(Self::OperatingSystem),
            'a' => Ok(Self::Application),
            _ => Err(CpeError::InvalidCpeType {
                value: val.to_owned(),
            }),
        }
    }
}

#[cfg(feature = "display")]
impl fmt::Display for CpeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Any => if f.alternate() {
                write!(f, "")
            } else {
                write!(f, "ANY")
            }
            Self::Hardware => write!(f, "h"),
            Self::OperatingSystem => write!(f, "o"),
            Self::Application => write!(f, "a"),

        }
    }
}
