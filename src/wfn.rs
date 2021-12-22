//! # CPE Well-Formed Name
//!
//! A CPE 2.3 WFN as defined in
//! [CPE-N:5.3](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf)
//!
//! The values of a `Wfn` can be set directly via the fields, but this is discouraged,
//! as there is no validation of the values. There are a few options for creating a `Wfn`:
//!
//! 1. Parsing a WFN string
//! ```
//! use cpe::wfn::Wfn;
//!
//! let cpe: Wfn = Wfn::parse(r#"wfn:[part="a",vendor="rust",product="cargo"]"#).unwrap();
//! println!("{:?}", cpe);
//! ```
//! 2. Using the builder pattern, with [builder](#method.builder)
//! 3. Using the "setter" methods
//! ```
//! use cpe::wfn::Wfn;
//!
//! let mut cpe: Wfn = Wfn::new();
//! cpe.set_part("a").unwrap();
//! cpe.set_vendor("rust").unwrap();
//! cpe.set_product("cargo").unwrap();
//!
//! println!("{:?}", cpe);
//! ```
//! 4. Using the `wfn!{}` macro
//! ```
//! use cpe::{wfn, wfn::Wfn};
//!
//! let cpe: Wfn = wfn!{
//!                   part: "a",
//!                   vendor: "rust",
//!                   product: "cargo",
//!               }.unwrap();
//!
//! println!("{:?}", cpe);
//! ```
//!
//!
//! ## Valid values
//! The valid values for WFN attributes come from the grammar for WFNs in [CPE-N:5.3.2]

use std::convert::TryFrom;
use std::fmt;

use crate::component::{Component, OwnedComponent};
use crate::cpe::{CpeType, Language};
use crate::error::{CpeError, Result};
use crate::uri::{OwnedUri, Uri};

use crate::builder::CpeBuilder;

fn split_unescaped_comma(s: &str) -> Vec<&str> {
    let indices = s
        .match_indices(',')
        .map(|(index, _)| index)
        .filter(|index| !s[..*index].ends_with('\\'))
        .collect::<Vec<_>>();
    let mut parts = Vec::with_capacity(indices.len() + 1);
    let mut last = None;
    for index in indices {
        if let Some(last) = last {
            parts.push(&s[last..index]);
        } else {
            parts.push(&s[..index]);
        }
        last = Some(index + 1);
    }
    if let Some(last) = last {
        parts.push(&s[last..]);
    } else {
        parts.push(s);
    }
    parts
}

/// Helper macro to create a `Wfn` from literal values.
#[macro_export]
macro_rules! wfn {
    ($($field:ident : $value:literal),*$(,)*) => {{
        let mut wfn = Wfn::builder();
        $(
            wfn.$field($value);
        )*
        wfn.validate()
    }}
}

/// A CPE 2.3 Well-Formed Name
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
/// use cpe::wfn::Wfn;
/// let wfn = Wfn::builder()
///           .part("a")
///           .vendor("foo\\!")
///           .validate()
///           .unwrap();
///
/// assert_eq!(format!("{}", wfn), "wfn:[part=a,vendor=foo!,product=ANY,version=ANY,update=ANY,edition=ANY,language=ANY,sw_edition=ANY,target_sw=ANY,target_hw=ANY,other=ANY]".to_owned());
/// assert_eq!(format!("{:#}", wfn), "wfn:[part=a,vendor=foo\\!,product=ANY,version=ANY,update=ANY,edition=ANY,language=ANY,sw_edition=ANY,target_sw=ANY,target_hw=ANY,other=ANY]".to_owned());
///```
///
/// Additionally, the `0` for zero-padding integers can be used to omit default "ANY" fields.
///```
/// use cpe::wfn::Wfn;
/// let wfn = Wfn::builder()
///           .part("a")
///           .vendor("foo\\!")
///           .validate()
///           .unwrap();
///
/// assert_eq!(format!("{:0}", wfn), "wfn:[part=a,vendor=foo!]".to_owned());
/// assert_eq!(format!("{:#0}", wfn), "wfn:[part=a,vendor=foo\\!]".to_owned());
///```
#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Wfn<'a> {
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

impl<'a> Wfn<'a> {
    /// Create a default Wfn with all fields set to ANY.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a `CpeBuilder` struct to construct a new Wfn.
    ///
    /// ```
    /// use cpe::wfn::Wfn;
    ///
    /// let cpe: Wfn = Wfn::builder()
    ///               .part("a")
    ///               .vendor("rust")
    ///               .product("cargo")
    ///               .validate()
    ///               .unwrap();
    ///
    /// println!("{:?}", cpe);
    /// ```
    pub fn builder() -> CpeBuilder<'a, Wfn<'a>> {
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
        self.vendor = Component::parse_wfn_field(vendor)?;
        Ok(())
    }

    /// Set the CPE product.
    ///
    /// The provided string slice will be parsed to its semantic meaning.
    pub fn set_product(&mut self, product: &'a str) -> Result<()> {
        self.product = Component::parse_wfn_field(product)?;
        Ok(())
    }

    /// Set the CPE product.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_version(&mut self, version: &'a str) -> Result<()> {
        self.version = Component::parse_wfn_field(version)?;
        Ok(())
    }

    /// Set the CPE update.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_update(&mut self, update: &'a str) -> Result<()> {
        self.update = Component::parse_wfn_field(update)?;
        Ok(())
    }

    /// Set the CPE edition.
    ///
    /// The provided string will be parsed to its semantic meaning.
    /// Note that this funciton will not unpack a packed `~` delimited edition component.
    pub fn set_edition(&mut self, edition: &'a str) -> Result<()> {
        self.edition = Component::parse_wfn_field(edition)?;
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

    /// Set the CPE software edition.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_sw_edition(&mut self, sw_edition: &'a str) -> Result<()> {
        self.sw_edition = Component::parse_wfn_field(sw_edition)?;
        Ok(())
    }

    /// Set the CPE target software.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_target_sw(&mut self, target_sw: &'a str) -> Result<()> {
        self.target_sw = Component::parse_wfn_field(target_sw)?;
        Ok(())
    }

    /// Set the CPE target hardware.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_target_hw(&mut self, target_hw: &'a str) -> Result<()> {
        self.target_hw = Component::parse_wfn_field(target_hw)?;
        Ok(())
    }

    /// Set the CPE "other" value.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_other(&mut self, other: &'a str) -> Result<()> {
        self.other = Component::parse_wfn_field(other)?;
        Ok(())
    }

    /// Create an Owned copy of this CPE WFN.
    pub fn to_owned(&self) -> OwnedWfn {
        self.into()
    }

    /// Create a `Uri`, perserving lifetimes of the original input.
    /// Note that strings may be cloned if the input was decoded.
    pub fn as_uri(&self) -> Uri<'a> {
        self.into()
    }

    /// Parse a CPE URI string.
    ///
    /// This function will decode percent encodings and special characters to their
    /// semantic meaning.
    pub fn parse(value: &'a str) -> Result<Self> {
        let offset = if value.starts_with("wfn:[") {
            5
        } else {
            return Err(CpeError::InvalidPrefix {
                value: value.to_owned(),
            });
        };

        let mut wfn = Self::new();

        let remainder = &value[offset..value.len() - 1];

        let mut set_part = false;
        let mut set_vendor = false;
        let mut set_product = false;
        let mut set_version = false;
        let mut set_update = false;
        let mut set_edition = false;
        let mut set_language = false;
        let mut set_sw_edition = false;
        let mut set_target_sw = false;
        let mut set_target_hw = false;
        let mut set_other = false;

        let parts = split_unescaped_comma(remainder);
        eprintln!("{:?}", parts);
        for part in parts {
            let (attribute, value) = {
                let mut parts = part.splitn(2, '=');
                let att = parts
                    .next()
                    .ok_or_else(|| CpeError::InvalidWfn {
                        value: value.to_owned(),
                        expected: format!("malformed attribute value pair `{}`", part),
                    })?
                    .to_lowercase();
                let val = parts
                    .next()
                    .ok_or_else(|| CpeError::InvalidWfn {
                        value: value.to_owned(),
                        expected: format!("malformed attribute value pair `{}`", part),
                    })?
                    .trim_start_matches('"')
                    .trim_end_matches('"');
                (att, val)
            };

            macro_rules! set_field {
                ($name:literal, $check:ident) => {
                    if $check {
                        return Err(CpeError::DuplicateAttribute {
                            value: value.to_owned(),
                            name: $name,
                        });
                    } else {
                        $check = true;
                        wfn.$check(value)?;
                    }
                };
            }

            match attribute.as_str() {
                "part" => set_field!("part", set_part),
                "language" => set_field!("language", set_language),
                "vendor" => set_field!("vendor", set_vendor),
                "product" => set_field!("product", set_product),
                "version" => set_field!("version", set_version),
                "update" => set_field!("update", set_update),
                "edition" => set_field!("edition", set_edition),
                "sw_edition" => set_field!("sw_edition", set_sw_edition),
                "target_sw" => set_field!("target_sw", set_target_sw),
                "target_hw" => set_field!("target_hw", set_target_hw),
                "other" => set_field!("other", set_other),
                _ => {
                    return Err(CpeError::InvalidWfn {
                        value: value.to_owned(),
                        expected: format!("invalid attribute `{}`", attribute),
                    })
                }
            }
        }

        Ok(wfn)
    }
}

impl fmt::Display for Wfn<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! write_field {
            ($field:ident) => {
                if !f.sign_aware_zero_pad() || &self.$field != &Component::Any {
                    if f.alternate() {
                        write!(f, ",{}={}", stringify!($field), self.$field.encode_wfn())?;
                    } else {
                        write!(f, ",{}={}", stringify!($field), self.$field)?;
                    }
                }
            };
        }
        write!(f, "wfn:[")?;
        write!(f, "part={}", self.part)?;
        write_field!(vendor);
        write_field!(product);
        write_field!(version);
        write_field!(update);
        write_field!(edition);
        if !f.sign_aware_zero_pad() || self.language != Language::Any {
            write!(f, ",language={}", self.language)?;
        }
        write_field!(sw_edition);
        write_field!(target_sw);
        write_field!(target_hw);
        write_field!(other);
        write!(f, "]")
    }
}

impl<'a> From<Uri<'a>> for Wfn<'a> {
    fn from(uri: Uri<'a>) -> Self {
        Self {
            part: uri.part,
            vendor: uri.vendor,
            product: uri.product,
            version: uri.version,
            update: uri.update,
            edition: uri.edition,
            language: uri.language,
            sw_edition: uri.sw_edition,
            target_sw: uri.target_sw,
            target_hw: uri.target_hw,
            other: uri.other,
        }
    }
}

impl<'a> From<&Uri<'a>> for Wfn<'a> {
    fn from(uri: &Uri<'a>) -> Self {
        Self {
            part: uri.part,
            vendor: uri.vendor.clone(),
            product: uri.product.clone(),
            version: uri.version.clone(),
            update: uri.update.clone(),
            edition: uri.edition.clone(),
            language: uri.language.clone(),
            sw_edition: uri.sw_edition.clone(),
            target_sw: uri.target_sw.clone(),
            target_hw: uri.target_hw.clone(),
            other: uri.other.clone(),
        }
    }
}

/// Owned copy of a Wfn for when lifetimes do not permit borrowing
/// from the input.
#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct OwnedWfn {
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

impl fmt::Display for OwnedWfn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! write_field {
            ($field:ident) => {
                if !f.sign_aware_zero_pad() || self.$field != OwnedComponent::Any {
                    if f.alternate() {
                        write!(
                            f,
                            ",{}={}",
                            stringify!($field),
                            self.$field.as_component().encode_wfn()
                        )?;
                    } else {
                        write!(f, ",{}={}", stringify!($field), self.$field.as_component())?;
                    }
                }
            };
        }
        write!(f, "wfn:[")?;
        write!(f, "part={}", self.part)?;
        write_field!(vendor);
        write_field!(product);
        write_field!(version);
        write_field!(update);
        write_field!(edition);
        if !f.sign_aware_zero_pad() || self.language != Language::Any {
            write!(f, ",language={}", self.language)?;
        }
        write_field!(sw_edition);
        write_field!(target_sw);
        write_field!(target_hw);
        write_field!(other);
        write!(f, "]")
    }
}

macro_rules! into {
    ($t:ty) => {
        impl From<$t> for OwnedWfn {
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
into!(OwnedUri);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_wfn() {
        let wfn = Wfn::parse(r#"wfn:[part="a",vendor="microsoft",product="internet_explorer",version="8\.0\.6001",update="beta",edition=NA]"#).unwrap();
        assert_eq!(wfn.part, CpeType::Application);
        assert_eq!(wfn.vendor, Component::new("microsoft"));
        assert_eq!(wfn.product, Component::new("internet_explorer"));
        assert_eq!(wfn.version, Component::new("8.0.6001"));
        assert_eq!(wfn.update, Component::new("beta"));
        assert_eq!(wfn.edition, Component::NotApplicable);
    }

    #[test]
    fn with_macro() {
        let wfn = Wfn::parse(r#"wfn:[part="a",vendor="microsoft",product="internet_explorer",version="8\.*",update="sp?",edition=NA,language=ANY]"#).unwrap();
        assert_eq!(
            wfn,
            wfn! {
                part: "a",
                vendor: "microsoft",
                product: "internet_explorer",
                version: r"8\.*",
                update: "sp?",
                edition: "NA",
            }
            .unwrap()
        );
        assert_eq!(
            wfn,
            wfn! {
                part: "a",
                vendor: "microsoft",
                product: "internet_explorer",
                version: r"8\.*",
                update: "sp?",
                edition: "NA",
                language: "ANY",
            }
            .unwrap()
        )
    }

    #[test]
    fn with_escaped_comma() {
        let wfn = Wfn::parse(r#"wfn:[part="a",vendor="micr\,osoft",product="internet_explorer",version="8\.*",update="sp?",edition=NA,language=ANY]"#).unwrap();
        assert_eq!(
            wfn,
            wfn! {
                part: "a",
                vendor: r"micr\,osoft",
                product: "internet_explorer",
                version: r"8\.*",
                update: "sp?",
                edition: "NA",
            }
            .unwrap()
        );
        assert_eq!(wfn.part, CpeType::Application);
        assert_eq!(wfn.vendor, Component::new("micr,osoft"));
        assert_eq!(wfn.product, Component::new("internet_explorer"));
        assert_eq!(wfn.version, Component::new("8.*"));
        assert_eq!(wfn.update, Component::new("sp?"));
        assert_eq!(wfn.edition, Component::NotApplicable);
    }
}
