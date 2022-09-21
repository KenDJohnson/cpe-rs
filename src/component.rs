//! # CPE Component
//!
//! The `Compenent` structs represent an avstring as defined in
//! [CPE-N:5.3](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf), or a CPE 2.2
//! component (one of vendor, product, version, etc.) as defined in
//! [CPE-N:6.1](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf),
//! Different functions are provided for parsing each TODO insert links.
//!
//! Note that these functions are primarily for internal use, and parsing the whole
//! WFN or URI (with TODO: links) is preferred.

use crate::error::Result;
use crate::parse::*;

use std::borrow::Cow;
use std::fmt;

pub type PackedComponents<'a> = (
    Component<'a>,
    Component<'a>,
    Component<'a>,
    Component<'a>,
    Component<'a>,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Component<'a> {
    Any,
    NotApplicable,
    Value(Cow<'a, str>),
}

impl fmt::Display for Component<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            match self {
                Self::Any => write!(f, "*"),
                Self::NotApplicable => write!(f, "?"),
                Self::Value(val) => write!(f, "{}", val),
            }
        } else {
            match self {
                Self::Any => write!(f, "ANY"),
                Self::NotApplicable => write!(f, "NA"),
                Self::Value(val) => write!(f, "{}", val),
            }
        }
    }
}

impl Default for Component<'_> {
    fn default() -> Self {
        Component::Any
    }
}

impl<'a> Component<'a> {
    pub fn new(val: &'a str) -> Self {
        if val == "-" || val == "NA" {
            Self::NotApplicable
        } else if val.is_empty() || val == "ANY" {
            Self::Any
        } else {
            Self::Value(Cow::Borrowed(val))
        }
    }

    pub fn new_string(val: String) -> Self {
        if val == "-" || val == "NA" {
            Self::NotApplicable
        } else if val.is_empty() || val == "ANY" {
            Self::Any
        } else {
            Self::Value(Cow::Owned(val))
        }
    }

    pub fn parse_wfn_field(value: &'a str) -> Result<Self> {
        parse_wfn_attribute(value)
    }

    pub fn parse_uri_field(value: &'a str) -> Result<Self> {
        parse_uri_attribute(value)
    }

    pub fn to_owned(&self) -> OwnedComponent {
        self.into()
    }

    pub fn encode_uri(&'a self) -> Cow<'a, str> {
        crate::parse::encode_uri_attribute(self)
    }

    pub fn encode_wfn(&'a self) -> Cow<'a, str> {
        crate::parse::encode_wfn_attribute(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OwnedComponent {
    Any,
    NotApplicable,
    Value(String),
}

impl Default for OwnedComponent {
    fn default() -> Self {
        Self::Any
    }
}

impl OwnedComponent {
    pub fn parse_wfn_field(value: &str) -> Result<Self> {
        Component::parse_wfn_field(value).map(|component| component.into())
    }

    pub fn parse_uri_field(value: &str) -> Result<Self> {
        Component::parse_uri_field(value).map(|component| component.into())
    }

    pub fn as_component(&self) -> Component {
        match self {
            Self::Any => Component::Any,
            Self::NotApplicable => Component::NotApplicable,
            Self::Value(s) => Component::Value(Cow::Borrowed(s.as_str())),
        }
    }

    pub(crate) fn to_owned(&self) -> OwnedComponent {
        self.clone()
    }
}

impl From<&Component<'_>> for OwnedComponent {
    fn from(attribute: &Component<'_>) -> Self {
        match attribute {
            Component::Any => Self::Any,
            Component::NotApplicable => Self::NotApplicable,
            Component::Value(value) => Self::Value((*value).to_owned().to_string()),
        }
    }
}

impl From<Component<'_>> for OwnedComponent {
    fn from(attribute: Component<'_>) -> Self {
        match attribute {
            Component::Any => Self::Any,
            Component::NotApplicable => Self::NotApplicable,
            Component::Value(value) => Self::Value((*value).to_owned()),
        }
    }
}
