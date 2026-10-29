//! # FSB - A CPE 2.3 Formatted String Binding
//!
//! CPE 2.3 Formatted String Bindings as specified in
//! [CPE23-N:6](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf).

use std::{convert::TryFrom, fmt::Display, str::FromStr};

use crate::{
    builder::CpeBuilder, component::{Component, OwnedComponent}, cpe::{CpeType, Language}, error::{CpeError, Result}, parse::{parse_fsb_attribute, validate_fsb_language}, uri::Uri, wfn::{OwnedWfn, Wfn},
};

/// A CPE 2.3 Formatted String Binding
///
/// Currently does not retain quoting information for leading or trailing '*' or '?'
#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Fsb<'a> {
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

impl Display for Fsb<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_field {
            ($field:ident) => {
                if &self.$field == &Component::Any {
                    write!(f, ":*")?;
                } else if &self.$field == &Component::NotApplicable {
                    write!(f, ":-")?;
                } else {
                    write!(f, ":{}", self.$field.encode_fsb())?;
                }
            };
        }
        write!(f, "cpe:2.3")?;
        write!(f, ":{:#}", self.part)?;
        write_field!(vendor);
        write_field!(product);
        write_field!(version);
        write_field!(update);
        write_field!(edition);
        write!(f, ":{:#}", self.language)?;
        write_field!(sw_edition);
        write_field!(target_sw);
        write_field!(target_hw);
        write_field!(other);
        Ok(())
    }
}

fn split_only_unescaped_separators(s: &str, separator: char, escape_char: char) -> Vec<&str> {
    let mut splits = vec![];

    let mut escap_char_count: usize = 0;
    let mut last_pos = 0;
    for (pos, c) in s.char_indices() {
        match c {
            c if c == escape_char => escap_char_count += 1,
            c if c == separator && escap_char_count % 2 == 0 => {
                splits.push(&s[last_pos..pos]);
                last_pos = pos + 1;
                escap_char_count = 0;
            }
            _ => escap_char_count = 0,
        }
    }
    splits.push(&s[last_pos..]);
    splits
}

impl<'a> Fsb<'a> {
    /// Create a new Fsb with default values of `ANY` for each attribute.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a `CpeBuilder` struct to construct a new Wfn.
    ///
    /// ```
    /// use cpe_rs_fork::fsb::Fsb;
    ///
    /// let cpe: Fsb = Fsb::builder()
    ///               .part("a")
    ///               .vendor("rust")
    ///               .product("cargo")
    ///               .validate()
    ///               .unwrap();
    ///
    /// println!("{:?}", cpe);
    /// ```
    pub fn builder() -> CpeBuilder<'a, Fsb<'a>> {
        CpeBuilder::default()
    }

    /// Set the CPE type part `a`, `o`, `h` or `*`.
    ///
    /// The provided string slice will be parsed to its semantic meaning
    pub fn set_part(&mut self, part: &str) -> Result<()> {
        self.part = CpeType::try_from(part)?;
        Ok(())
    }

    /// Set the CPE vendor.
    ///
    /// The provided string slice will be parsed to its semantic meaning.
    pub fn set_vendor(&mut self, vendor: &'a str) -> Result<()> {
        self.vendor = Component::parse_fsb_field(vendor)?;
        Ok(())
    }

    /// Set the CPE product.
    ///
    /// The provided string slice will be parsed to its semantic meaning.
    pub fn set_product(&mut self, product: &'a str) -> Result<()> {
        self.product = Component::parse_fsb_field(product)?;
        Ok(())
    }

    /// Set the CPE product.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_version(&mut self, version: &'a str) -> Result<()> {
        self.version = Component::parse_fsb_field(version)?;
        Ok(())
    }

    /// Set the CPE update.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_update(&mut self, update: &'a str) -> Result<()> {
        self.update = Component::parse_fsb_field(update)?;
        Ok(())
    }

    /// Set the CPE edition.
    ///
    /// The provided string will be parsed to its semantic meaning.
    /// Note that this function will not unpack a packed `~` delimited edition component.
    pub fn set_edition(&mut self, edition: &'a str) -> Result<()> {
        self.edition = Component::parse_fsb_field(edition)?;
        Ok(())
    }

    /// Set the CPE language.
    ///
    /// The provided string will be parsed to its semantic meaning.
    /// `language` must be a valid RFC-5646 language tag.
    pub fn set_language(&mut self, language: &'a str) -> Result<()> {
        if !validate_fsb_language(language) {
            return Err(CpeError::InvalidAttribute { value: language.to_string() });
        }
        self.language = if language == "*" {
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
        self.sw_edition = Component::parse_fsb_field(sw_edition)?;
        Ok(())
    }

    /// Set the CPE target software.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_target_sw(&mut self, target_sw: &'a str) -> Result<()> {
        self.target_sw = Component::parse_fsb_field(target_sw)?;
        Ok(())
    }

    /// Set the CPE target hardware.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_target_hw(&mut self, target_hw: &'a str) -> Result<()> {
        self.target_hw = Component::parse_fsb_field(target_hw)?;
        Ok(())
    }

    /// Set the CPE "other" value.
    ///
    /// The provided string will be parsed to its semantic meaning.
    pub fn set_other(&mut self, other: &'a str) -> Result<()> {
        self.other = Component::parse_fsb_field(other)?;
        Ok(())
    }

    /// Create an Owned copy of this CPE FSB
    pub fn to_owned(&self) -> OwnedFsb {
        self.into()
    }

    /// Create a `Uri`, perserving lifetimes of the original input.
    /// Note that strings may be cloned if the input was decoded.
    pub fn as_uri(&self) -> Uri<'a> {
        self.into()
    }

    /// Create a `Wfn`, perserving lifetimes of the original input.
    /// Note that strings may be cloned if the input was decoded.
    pub fn as_wfn(&self) -> Wfn<'a> {
        self.into()
    }

    /// Parse a CPE formatted string.
    ///
    /// This function will decode special characters to their semantic meaning.
    pub fn parse(fsb: &'a str) -> Result<Self> {
        let stripped_fsb = match fsb.strip_prefix("cpe:2.3:") {
            Some(f) => f,
            None => {
                return Err(CpeError::InvalidPrefix {
                    value: fsb.to_owned(),
                });
            }
        };

        // It is necessary to prevent splitting on escaped separators
        let components = split_only_unescaped_separators(stripped_fsb, ':', '\\');
        let mut components = components.iter();

        let part = if let Some(part) = components.next() {
            CpeType::try_from(*part)?
        } else {
            return Err(CpeError::UnexpectedEnd {
                value: fsb.to_owned(),
                expected: "part".to_string(),
            });
        };

        macro_rules! parse_field {
            ($attribute:expr) => {
                if let Some(value) = components.next() {
                    parse_fsb_attribute(value)?
                } else {
                    return Err(CpeError::UnexpectedEnd {
                        value: fsb.to_owned(),
                        expected: $attribute.to_string(),
                    });
                }
            };
        }

        let vendor = parse_field!("vendor");
        let product = parse_field!("product");
        let version = parse_field!("version");
        let update = parse_field!("update");
        let edition = parse_field!("edition");

        let language = if let Some(language) = components.next() {
            if !validate_fsb_language(language) {
                return Err(CpeError::InvalidAttribute { value: language.to_string() });
            }
            if *language == "*" {
                Language::Any
            } else {
                Language::Language(language.parse()?)
            }
        } else {
            return Err(CpeError::UnexpectedEnd {
                value: fsb.to_owned(),
                expected: "language".to_string(),
            });
        };

        let sw_edition = parse_field!("sw_edition");
        let target_sw = parse_field!("target_sw");
        let target_hw = parse_field!("target_hw");
        let other = parse_field!("other");

        if components.next().is_some() {
            return Err(CpeError::InvalidFsb {
                value: fsb.to_owned(),
                reason: "More than the expected 11 attributes were found seperated by `:`",
            });
        }

        Ok(Self {
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

impl<'a> From<Wfn<'a>> for Fsb<'a> {
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

impl<'a> From<&Wfn<'a>> for Fsb<'a> {
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

impl<'a> From<Uri<'a>> for Fsb<'a> {
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

impl<'a> From<&Uri<'a>> for Fsb<'a> {
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

/// Owned copy of a FSB for when lifetimes do not permit borrowing from the input.
#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct OwnedFsb {
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

impl Display for OwnedFsb {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_field {
            ($field:ident) => {
                if &self.$field == &OwnedComponent::Any {
                    write!(f, ":*")?;
                } else if &self.$field == &OwnedComponent::NotApplicable {
                    write!(f, ":-")?;
                } else {
                    write!(f, ":{}", self.$field.as_component().encode_fsb())?;
                }
            };
        }
        write!(f, "cpe:2.3")?;
        write!(f, ":{:#}", self.part)?;
        write_field!(vendor);
        write_field!(product);
        write_field!(version);
        write_field!(update);
        write_field!(edition);
        write!(f, ":{:#}", self.language)?;
        write_field!(sw_edition);
        write_field!(target_sw);
        write_field!(target_hw);
        write_field!(other);
        Ok(())
    }
}

impl FromStr for OwnedFsb {
    type Err = CpeError;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        Fsb::parse(s).map(|fsb| fsb.to_owned())
    }
}

macro_rules! into {
    ($t:ty) => {
        impl From<$t> for OwnedFsb {
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

into!(Fsb<'_>);
into!(&Fsb<'_>);
into!(Wfn<'_>);
into!(&Wfn<'_>);
into!(OwnedWfn);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn split_escape_parity() {
        let split = |s| split_only_unescaped_separators(s, ':', '\\');
        assert_eq!(split(r"a:b"), vec!["a", "b"]);
        assert_eq!(split(r"a\:b"), vec![r"a\:b"]); // escaped colon
        assert_eq!(split(r"a\\:b"), vec![r"a\\", "b"]); // escaped backslash, then a real separator
        assert_eq!(split(r"a\\\:b"), vec![r"a\\\:b"]); // escaped backslash + escaped colon
        assert_eq!(split(r"a\:b\:c"), vec![r"a\:b\:c"]); // case that fails without the reset
        assert_eq!(split("ä:b"), vec!["ä", "b"]); // multi-byte character
    }

    #[test]
    fn basic_fsb() {
        assert_eq!(
            Fsb::parse("cpe:2.3:a:microsoft:internet_explorer:8.0.6001:beta:*:*:*:*:*:*").unwrap(),
            Fsb::builder()
                .part("a")
                .vendor("microsoft")
                .product("internet_explorer")
                .version("8.0.6001")
                .update("beta")
                .validate()
                .unwrap()
        );
        assert_eq!(
            Fsb::parse("cpe:2.3:a:microsoft:internet_explorer:8.*:sp?:*:*:*:*:*:*").unwrap(),
            Fsb::builder()
                .part("a")
                .vendor("microsoft")
                .product("internet_explorer")
                .version("8.*")
                .update("sp?")
                .validate()
                .unwrap()
        );
        assert_eq!(
            Fsb::parse("cpe:2.3:a:hp:insight:7.4.0.1570:-:*:*:online:win2003:x64:*").unwrap(),
            Fsb::builder()
                .part("a")
                .vendor("hp")
                .product("insight")
                .version("7.4.0.1570")
                .update("-")
                .sw_edition("online")
                .target_sw("win2003")
                .target_hw("x64")
                .validate()
                .unwrap()
        );
        assert_eq!(
            Fsb::parse("cpe:2.3:a:hp:openview_network_manager:7.51:*:*:*:*:linux:*:*").unwrap(),
            Fsb::builder()
                .part("a")
                .vendor("hp")
                .product("openview_network_manager")
                .version("7.51")
                .target_sw("linux")
                .validate()
                .unwrap()
        );
        assert_eq!(
            Fsb::parse("cpe:2.3:a:foo\\\\bar:big\\$money_2010:*:*:*:*:special:ipod_touch:80gb:*")
                .unwrap(),
            Fsb::builder()
                .part("a")
                .vendor("foo\\\\bar")
                .product("big\\$money_2010")
                .sw_edition("special")
                .target_sw("ipod_touch")
                .target_hw("80gb")
                .validate()
                .unwrap()
        );
    }

    #[test]
    fn dont_split_on_escaped_separator() {
        assert_eq!(
            Fsb::parse(r"cpe:2.3:a:micro\:soft:internet_explorer:8.0.6001:beta:*:*:*:*:*:*")
                .unwrap(),
            Fsb::builder()
                .part("a")
                .vendor(r"micro\:soft")
                .product("internet_explorer")
                .version("8.0.6001")
                .update("beta")
                .validate()
                .unwrap()
        );
    }

    #[test]
    fn split_on_unescaped_separator_with_preceeding_escaped_backslash() {
        assert_eq!(
            Fsb::parse(r"cpe:2.3:a:microsoft\\:internet_explorer:8.0.6001:beta:*:*:*:*:*:*")
                .unwrap(),
            Fsb::builder()
                .part("a")
                .vendor(r"microsoft\\")
                .product("internet_explorer")
                .version("8.0.6001")
                .update("beta")
                .validate()
                .unwrap()
        );
    }

    #[test]
    fn ser_de_roundtrip() {
        let cpe = "cpe:2.3:a:microsoft:internet_explorer:8.0.6001:beta:*:*:*:*:*:*";
        assert_eq!(Fsb::parse(cpe).unwrap().to_string(), cpe);
        let cpe = "cpe:2.3:a:microsoft:internet_explorer:8.*:sp?:*:*:*:*:*:*";
        assert_eq!(
            Fsb::parse(cpe).unwrap().to_string(),
            "cpe:2.3:a:microsoft:internet_explorer:8.*:sp?:*:*:*:*:*:*"
        );
        let cpe = "cpe:2.3:a:hp:insight:7.4.0.1570:-:*:*:online:win2003:x64:*";
        assert_eq!(Fsb::parse(cpe).unwrap().to_string(), cpe);
        let cpe = "cpe:2.3:a:hp:openview_network_manager:7.51:*:*:*:*:linux:*:*";
        assert_eq!(Fsb::parse(cpe).unwrap().to_string(), cpe);
        let cpe = r"cpe:2.3:a:foo\\bar:big\$money_2010:*:*:*:*:special:ipod_touch:80gb:*";
        assert_eq!(Fsb::parse(cpe).unwrap().to_string(), cpe);
    }
}
