//! # Uri - A CPE 2.2 URI
//!
//! CPE 2.2 URI as specified in [CPE22](https://cpe.mitre.org/files/cpe-specification_2.2.pdf) or in
//! [CPE23-N:6](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf)
//!
//! ## Valid values
//! The specification for a URI from [CPE23-N:6](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf)
//! defines both a strict grammar, and a more permissive grammar. The URIs are parsed with the permissive
//! grammar.
//!
//! ### Prefixes
//!
//! Some vendors use a prefix to denote a CPE that is not in a CPE dictionary. This is a two character
//! prefix before the leading `cpe:/`, typically in the form of `x-cpe:/` or `p-cpe:/`. A URI in this form
//! can be parsed with the `parse_prefix` function. There is no function to parse prefixed URIs using the
//! strict grammar.

use crate::builder::CpeBuilder;
use crate::component::{Component, OwnedComponent};
use crate::cpe::{CpeType, Language};
use crate::error::{CpeError, Result};
use crate::parse::{parse_packed_uri_attribute, parse_uri_attribute};
use crate::wfn::{OwnedWfn, Wfn};

use std::fmt;

use std::convert::TryFrom;

/// Helper macro to create a `Uri` from literal values.
#[macro_export]
macro_rules! uri {
    ($($field:ident : $value:literal),*$(,)*) => {{
        let mut uri = Uri::builder();
        $(
            wfn.$field($value);
        )*
            wfn.validate()
    }}
}

/// A CPE 2.2 URI
///
/// Note: field access is limited to ensure values only contain
/// semantically valid components. Fields can be accessed through the
/// "getter" methods, or through the [Cpe](../cpe/trait.Cpe.html) methods,
/// although the former is preferred with a `Cpe` as opposed to an `OwnedCpe`.
///
/// Display is implemented to show the decoded contents by default, or to re-encode
/// the components when `#` is used to specify an alternate.
///
///```
/// use cpe::uri::Uri;
/// let uri = Uri::builder()
///           .part("a")
///           .vendor("foo%21")
///           .validate()
///           .unwrap();
///
/// assert_eq!(format!("{}", uri), "cpe:/a:foo!:*:*:*:*:*".to_owned());
/// assert_eq!(format!("{:#}", uri), "cpe:/a:foo%21:*:*:*:*:*".to_owned());
///
///```
///
/// Additionally, the `0` for zero-padding integers can be used to omit default "*" fields.
///```
/// use cpe::uri::Uri;
/// let uri = Uri::builder()
///           .part("a")
///           .vendor("foo%21")
///           .validate()
///           .unwrap();
///
/// assert_eq!(format!("{:0}", uri), "cpe:/a:foo!".to_owned());
/// assert_eq!(format!("{:#0}", uri), "cpe:/a:foo%21".to_owned());
///```
#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Uri<'a> {
    pub(crate) part: CpeType,
    pub(crate) vendor: Component<'a>,
    pub(crate) product: Component<'a>,
    pub(crate) version: Component<'a>,
    pub(crate) update: Component<'a>,
    pub(crate) edition: Component<'a>,
    pub(crate) language: Language,
    pub(crate) sw_edition: Component<'a>,
    pub(crate) target_sw: Component<'a>,
    pub(crate) target_hw: Component<'a>,
    pub(crate) other: Component<'a>,
}

impl fmt::Display for Uri<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! write_field {
            ($field:ident) => {
                if !f.sign_aware_zero_pad() || &self.$field != &Component::Any {
                    if f.alternate() {
                        write!(f, ":{}", self.$field.encode_uri())?;
                    } else {
                        write!(f, ":{:#}", self.$field)?;
                    }
                }
            };
        }
        write!(f, "cpe:/")?;
        write!(f, "{:#}", self.part)?;
        write_field!(vendor);
        write_field!(product);
        write_field!(version);
        write_field!(update);
        if self.sw_edition != Component::Any
            || self.target_sw != Component::Any
            || self.target_hw != Component::Any
            || self.other != Component::Any
        {
            if f.alternate() {
                write!(
                    f,
                    "~{}~{}~{}~{}~{}",
                    self.edition.encode_uri(),
                    self.sw_edition.encode_uri(),
                    self.target_sw.encode_uri(),
                    self.target_hw.encode_uri(),
                    self.other.encode_uri()
                )?;
            } else {
                write!(
                    f,
                    "~{:#}~{:#}~{:#}~{:#}~{:#}",
                    self.edition, self.sw_edition, self.target_sw, self.target_hw, self.other
                )?;
            }
        } else {
            write_field!(edition);
        }
        if !f.sign_aware_zero_pad() || self.language != Language::Any {
            write!(f, ":{:#}", self.language)?;
        }
        Ok(())
    }
}

impl<'a> Uri<'a> {
    /// Create a new Uri with default values of `ANY` for each attribute.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a `CpeBuilder` struct to construct a new Wfn.
    ///
    /// ```
    /// use cpe::uri::Uri;
    ///
    /// let cpe: Uri = Uri::builder()
    ///               .part("a")
    ///               .vendor("rust")
    ///               .product("cargo")
    ///               .validate()
    ///               .unwrap();
    ///
    /// println!("{:?}", cpe);
    /// ```
    pub fn builder() -> CpeBuilder<'a, Uri<'a>> {
        CpeBuilder::default()
    }

    /// Set the CPE type part, `a`, `o`, `h`, or `*`.
    ///
    /// The provided string slice will be parsed to its semantic meaning.
    pub fn set_part(&mut self, part: &'a str) -> Result<()> {
        self.part = CpeType::try_from(part)?;
        Ok(())
    }

    /// Set the CPE vendor.
    ///
    /// The provided string slice will be parsed to its semantic meaning.
    pub fn set_vendor(&mut self, vendor: &'a str) -> Result<()> {
        self.vendor = Component::parse_uri_field(vendor)?;
        Ok(())
    }

    /// Set the CPE product.
    ///
    /// The provided string slice will be parsed to its semantic meaning.
    pub fn set_product(&mut self, product: &'a str) -> Result<()> {
        self.product = Component::parse_uri_field(product)?;
        Ok(())
    }

    /// Set the CPE product.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_version(&mut self, version: &'a str) -> Result<()> {
        self.version = Component::parse_uri_field(version)?;
        Ok(())
    }

    /// Set the CPE update.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_update(&mut self, update: &'a str) -> Result<()> {
        self.update = Component::parse_uri_field(update)?;
        Ok(())
    }

    /// Set the CPE edition.
    ///
    /// The provided string will be parsed to its semantic meaning.
    /// Note that this funciton will unpack a packed `~` delimited edition component if applicable.
    pub fn set_edition(&mut self, edition: &'a str) -> Result<()> {
        let (edition, sw_edition, target_sw, target_hw, other) =
            parse_packed_uri_attribute(edition)?;
        self.edition = edition;
        self.sw_edition = sw_edition;
        self.target_sw = target_sw;
        self.target_hw = target_hw;
        self.other = other;
        Ok(())
    }

    /// Set the CPE language.
    ///
    /// The provided string will be parsed to its semantic meaning.
    /// `language` must be a valid RFC-5646 language tag.
    pub fn set_language(&mut self, language: &'a str) -> Result<()> {
        self.language = if language == "ANY" {
            Language::Any
        } else {
            Language::Language(language.parse()?)
        };
        Ok(())
    }

    /// Create an Owned copy of this CPE URI.
    pub fn to_owned(&self) -> OwnedUri {
        self.into()
    }

    /// Create a `Wfn`, perserving lifetimes of the original input.
    /// Note that strings may be cloned if the input was decoded.
    pub fn as_uri(&self) -> Wfn<'a> {
        self.into()
    }

    /// Parse a CPE URI string.
    ///
    /// This function will decode percent encodings and special characters to their
    /// semantic meaning.
    pub fn parse(uri: &'a str) -> Result<Self> {
        let uri = if uri.starts_with("x-") || uri.starts_with("p-") {
            &uri[2..]
        } else {
            uri
        };

        let uri = if uri.starts_with("cpe:/") {
            &uri[5..]
        } else {
            return Err(CpeError::InvalidPrefix {
                value: uri.to_owned(),
            });
        };

        let mut components = uri.split(':');

        let part = if let Some(part) = components.next() {
            CpeType::try_from(part)?
        } else {
            return Ok(Self::default());
        };

        macro_rules! parse_field {
            (parsed: $($parsed:ident),+) => {
                if let Some(c) = components.next() {
                    parse_uri_attribute(c)?
                } else {
                    return Ok(Self{ $($parsed, )* ..Default::default() });
                }
            }
        }

        let vendor = parse_field!(parsed: part);
        let product = parse_field!(parsed: part, vendor);
        let version = parse_field!(parsed: part, vendor, product);
        let update = parse_field!(parsed: part, vendor, product, version);

        let (edition, sw_edition, target_sw, target_hw, other) = components
            .next()
            .map(|edition| parse_packed_uri_attribute(edition))
            .transpose()?
            .unwrap_or_default();

        let language = if let Some(c) = components.next() {
            Language::Language(c.parse()?)
        } else {
            Language::Any
        };

        Ok(Uri {
            part,
            vendor,
            product,
            version,
            update,
            edition,
            language,
            sw_edition,
            target_sw,
            target_hw,
            other,
        })
    }
}

impl<'a> From<Wfn<'a>> for Uri<'a> {
    fn from(wfn: Wfn<'a>) -> Self {
        Self {
            part: wfn.part,
            vendor: wfn.vendor,
            product: wfn.product,
            version: wfn.version,
            update: wfn.update,
            edition: wfn.edition,
            language: wfn.language,
            sw_edition: wfn.sw_edition,
            target_sw: wfn.target_sw,
            target_hw: wfn.target_hw,
            other: wfn.other,
        }
    }
}

impl<'a> From<&Wfn<'a>> for Uri<'a> {
    fn from(wfn: &Wfn<'a>) -> Self {
        Self {
            part: wfn.part,
            vendor: wfn.vendor.clone(),
            product: wfn.product.clone(),
            version: wfn.version.clone(),
            update: wfn.update.clone(),
            edition: wfn.edition.clone(),
            language: wfn.language.clone(),
            sw_edition: wfn.sw_edition.clone(),
            target_sw: wfn.target_sw.clone(),
            target_hw: wfn.target_hw.clone(),
            other: wfn.other.clone(),
        }
    }
}

/// Owned copy of a URI for when lifetimes do not permit borrowing
/// from the input.
#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct OwnedUri {
    pub(crate) part: CpeType,
    pub(crate) vendor: OwnedComponent,
    pub(crate) product: OwnedComponent,
    pub(crate) version: OwnedComponent,
    pub(crate) update: OwnedComponent,
    pub(crate) edition: OwnedComponent,
    pub(crate) language: Language,
    pub(crate) sw_edition: OwnedComponent,
    pub(crate) target_sw: OwnedComponent,
    pub(crate) target_hw: OwnedComponent,
    pub(crate) other: OwnedComponent,
}

impl fmt::Display for OwnedUri {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! write_field {
            ($field:ident) => {
                if !f.sign_aware_zero_pad() || self.$field != OwnedComponent::Any {
                    if f.alternate() {
                        write!(f, ":{}", self.$field.as_component().encode_uri())?;
                    } else {
                        write!(f, ":{:#}", self.$field.as_component())?;
                    }
                }
            };
        }
        write!(f, "cpe:/")?;
        write!(f, "{:#}", self.part)?;
        write_field!(vendor);
        write_field!(product);
        write_field!(version);
        write_field!(update);
        if self.sw_edition != OwnedComponent::Any
            || self.target_sw != OwnedComponent::Any
            || self.target_hw != OwnedComponent::Any
            || self.other != OwnedComponent::Any
        {
            if f.alternate() {
                write!(
                    f,
                    "~{}~{}~{}~{}~{}",
                    self.edition.as_component().encode_uri(),
                    self.sw_edition.as_component().encode_uri(),
                    self.target_sw.as_component().encode_uri(),
                    self.target_hw.as_component().encode_uri(),
                    self.other.as_component().encode_uri()
                )?;
            } else {
                write!(
                    f,
                    "~{:#}~{:#}~{:#}~{:#}~{:#}",
                    self.edition.as_component(),
                    self.sw_edition.as_component(),
                    self.target_sw.as_component(),
                    self.target_hw.as_component(),
                    self.other.as_component()
                )?;
            }
        } else {
            write_field!(edition);
        }
        if !f.sign_aware_zero_pad() || self.language != Language::Any {
            write!(f, ":{:#}", self.language)?;
        }
        Ok(())
    }
}

macro_rules! into {
    ($t:ty) => {
        impl From<$t> for OwnedUri {
            fn from(cpe: $t) -> Self {
                Self {
                    part: cpe.part,
                    vendor: cpe.vendor.to_owned(),
                    product: cpe.product.to_owned(),
                    version: cpe.version.to_owned(),
                    update: cpe.update.to_owned(),
                    edition: cpe.edition.to_owned(),
                    language: cpe.language.clone(),
                    sw_edition: cpe.sw_edition.to_owned(),
                    target_sw: cpe.target_sw.to_owned(),
                    target_hw: cpe.target_hw.to_owned(),
                    other: cpe.other.to_owned(),
                }
            }
        }
    };
}

into!(Uri<'_>);
into!(&Uri<'_>);
into!(Wfn<'_>);
into!(&Wfn<'_>);
into!(OwnedWfn);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_uri() {
        assert_eq!(
            Uri::parse("cpe:/a:microsoft:internet_explorer:8.0.6001:beta").unwrap(),
            Uri::builder()
                .part("a")
                .vendor("microsoft")
                .product("internet_explorer")
                .version("8.0.6001")
                .update("beta")
                .validate()
                .unwrap()
        );
        let cpe = Uri::parse("cpe:/a:microsoft:internet_explorer:8.%02:sp%01").unwrap();
        assert_eq!(
            cpe,
            Uri::builder()
                .part("a")
                .vendor("microsoft")
                .product("internet_explorer")
                .version("8.%02")
                .update("sp%01")
                .validate()
                .unwrap()
        );
        assert_eq!(cpe.version, Component::new("8.*"));
        assert_eq!(cpe.update, Component::new("sp?"));
        let cpe =
            Uri::parse("cpe:/a:hp:insight_diagnostics:7.4.0.1570:-:~~online~win2003~x64~").unwrap();
        assert_eq!(
            cpe,
            Uri::builder()
                .part("a")
                .vendor("hp")
                .product("insight_diagnostics")
                .version("7.4.0.1570")
                .update("-")
                .edition("~~online~win2003~x64~")
                .validate()
                .unwrap()
        );
        assert_eq!(
            cpe,
            crate::wfn::Wfn::builder()
                .part("a")
                .vendor("hp")
                .product("insight_diagnostics")
                .version("7\\.4\\.0\\.1570")
                .update("NA")
                .sw_edition("online")
                .target_sw("win2003")
                .target_hw("x64")
                .validate()
                .unwrap()
                .into()
        );
    }
}
