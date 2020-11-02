//! General CPE utilities
use language_tags::LanguageTag;

use crate::component::Component;
use crate::error::{CpeError, Result};
use crate::uri::*;
use crate::wfn::*;

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

use std::fmt;
impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Any => {
                if f.alternate() {
                    write!(f, "*")
                } else {
                    write!(f, "ANY")
                }
            }
            Self::Language(tag) => write!(f, "{}", tag),
        }
    }
}

/// Generic accesss to fields in a Uri or Wfn (either owned or borrowed).
/// If you have a borrowed `Uri` or `Wfn`, it is prefereble to use the methods
/// from the struct directly instead of this trait, as they return a `&Component`
/// instead of `Component`, and in the case that the string value of a component
/// required decoding, using this trait may result in a string clone.
pub trait Cpe {
    fn part(&self) -> CpeType;
    fn vendor(&self) -> Component;
    fn product(&self) -> Component;
    fn version(&self) -> Component;
    fn update(&self) -> Component;
    fn edition(&self) -> Component;
    fn language(&self) -> &Language;
    fn sw_edition(&self) -> Component;
    fn target_sw(&self) -> Component;
    fn target_hw(&self) -> Component;
    fn other(&self) -> Component;
}

macro_rules! impl_cpe {
    ($t:ty) => {
        impl Cpe for $t {
            fn part(&self) -> CpeType {
                self.part
            }
            fn vendor(&self) -> Component {
                self.vendor.as_component()
            }
            fn product(&self) -> Component {
                self.product.as_component()
            }
            fn version(&self) -> Component {
                self.version.as_component()
            }
            fn update(&self) -> Component {
                self.update.as_component()
            }
            fn edition(&self) -> Component {
                self.edition.as_component()
            }
            fn language(&self) -> &Language {
                &self.language
            }
            fn sw_edition(&self) -> Component {
                self.sw_edition.as_component()
            }
            fn target_sw(&self) -> Component {
                self.target_sw.as_component()
            }
            fn target_hw(&self) -> Component {
                self.target_hw.as_component()
            }
            fn other(&self) -> Component {
                self.other.as_component()
            }
        }
    };
    ($t:ty, $_l:lifetime) => {
        impl Cpe for $t {
            fn part(&self) -> CpeType {
                self.part
            }
            fn vendor(&self) -> Component {
                self.vendor.clone()
            }
            fn product(&self) -> Component {
                self.product.clone()
            }
            fn version(&self) -> Component {
                self.version.clone()
            }
            fn update(&self) -> Component {
                self.update.clone()
            }
            fn edition(&self) -> Component {
                self.edition.clone()
            }
            fn language(&self) -> &Language {
                &self.language
            }
            fn sw_edition(&self) -> Component {
                self.sw_edition.clone()
            }
            fn target_sw(&self) -> Component {
                self.target_sw.clone()
            }
            fn target_hw(&self) -> Component {
                self.target_hw.clone()
            }
            fn other(&self) -> Component {
                self.other.clone()
            }
        }
    };
}

impl_cpe!(OwnedUri);
impl_cpe!(OwnedWfn);
impl_cpe!(Uri<'_>, 'a);
impl_cpe!(Wfn<'_>, 'a);

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

impl fmt::Display for CpeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Any => {
                if f.alternate() {
                    write!(f, "*")
                } else {
                    write!(f, "ANY")
                }
            }
            Self::Hardware => write!(f, "h"),
            Self::OperatingSystem => write!(f, "o"),
            Self::Application => write!(f, "a"),
        }
    }
}
