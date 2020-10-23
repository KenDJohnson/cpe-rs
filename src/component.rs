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
use crate::parse::ComponentStringParser;
use crate::uri::Uri;
use crate::wfn::Wfn;

use std::borrow::Cow;

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

impl Default for Component<'_> {
    fn default() -> Self {
        Component::Any
    }
}

impl<'a> Component<'a> {
    pub fn new(val: &'a str) -> Self {
        if val == "-" || val == "NA" {
            Self::NotApplicable
        } else if val == "" || val == "ANY" {
            Self::Any
        } else {
            Self::Value(Cow::Borrowed(val))
        }
    }

    pub fn new_string(val: String) -> Self {
        if val == "-" || val == "NA" {
            Self::NotApplicable
        } else if val == "" || val == "ANY" {
            Self::Any
        } else {
            Self::Value(Cow::Owned(val))
        }
    }

    pub fn parse_wfn_field(value: &'a str) -> Result<Self> {
        let mut parser = ComponentStringParser::<Wfn>::new(value);
        parser.parse()
    }

    pub fn parse_uri_field(value: &'a str) -> Result<Self> {
        let mut parser = ComponentStringParser::<Uri>::new(value);
        parser.parse()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OwnedComponent {
    Any,
    NotApplicable,
    Value(String),
}

impl OwnedComponent {
    pub fn parse_wfn_field(value: &str) -> Result<Self> {
        Component::parse_wfn_field(value).map(|component| component.into())
    }

    pub fn parse_uri_field(value: &str) -> Result<Self> {
        Component::parse_uri_field(value).map(|component| component.into())
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
