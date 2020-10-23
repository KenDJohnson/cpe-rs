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
//! 2. Using the builder pattern:
//! ```
//! use cpe::wfn::Wfn;
//!
//! let cpe: Wfn = Wfn::builder()
//!               .part("a")
//!               .vendor("rust")
//!               .product("cargo")
//!               .validate()
//!               .unwrap();
//!
//! println!("{:?}", cpe);
//! ```
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
//! ## Grammar
//! ABNF Grammar for WFNs from [CPE-N:5.3.2]
//!
//! ```ignore
//! avstring = ( body / ( spec_chrs *body2 )) *1spec_chrs
//! spec_chrs = 1*spec1 / spec2
//! spec1 = "?"
//! spec2 = "*"
//! body = ( body1 *body2 ) / 2*body2
//! body1 = unreserved / quoted1
//! body2 = unreserved / quoted2
//! unreserved = ALPHA / DIGIT / "_"
//! quoted1 = escape (escape / special / punc-no-dash)
//! quoted2 = escape (escape / special / punc-w-dash)
//! escape = "\"
//! special = spec1 / spec2
//! dash = "-"
//! punc-no-dash = "!" / DQUOTE / "#"/ "$"/ "%" / "&" / "'" /
//!                "(" / ")" / "+" / "," / "." / "/" /
//!                ":" / ";" / "<" / "=" / ">" / "@" / "[" /
//!                "]" / "^" / "`" / "{" / "|" / "}" / "~"
//! punc-w-dash = punc-no-dash / dash
//! DQUOTE = %x22 ; double quote
//! ALPHA = %x41-5a / %x61-7A ; A-Z or a-z
//! DIGIT = %x30-39 ; 0-9
//! ```
//!
//!
//! unreserved = \w
//! punc-no-dash = [!"#$%&'()+,./:;<=>@\[\]^`{|}~]
//! punc-w-dash = [!"#$%&'()+,./:;<=>@\[\]^`{|}~-]
//!
//! special = [?*]
//!
//! quoted1 = \\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~?*]
//! quoted2 = \\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~-?*]
//!
//! body1 = (?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~?*])
//! body2 = (?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~-?*])
//!
//!
//! body = (?:(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~?*](?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~-?*])*)|(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~-?*]){2,})
//!
//! avstring = (?:(?:(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~?*](?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~-?*])*)|(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~-?*]){2,})|(?:(?:\?{1,}|\*)(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~-?*])*))(?:\?{1,}|\*)?
//!
//!
use language_tags::LanguageTag;

use std::convert::TryFrom;

use crate::component::{Component, OwnedComponent};
use crate::cpe::{Language, CpeType};
use crate::error::{CpeError, Result};
use crate::parse::ComponentStringParser;

use crate::builder::CpeBuilder;

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

#[derive(Default, Debug, PartialEq)]
pub struct Wfn<'a> {
    pub part: CpeType,
    pub vendor: Component<'a>,
    pub product: Component<'a>,
    pub version: Component<'a>,
    pub update: Component<'a>,
    pub edition: Component<'a>,
    /// The language tag, in RFC 5646 (aka BCP47) format, as documented [here](https://tools.ietf.org/html/bcp47).
    /// Note that `cpe-rs` currently uses this for CPE v2.2 and CPE v2.3, despite CPE v2.2 using RFC 4646
    /// language tags, which were obsoleted by RFC 5646.
    pub language: Language,
    pub sw_edition: Component<'a>,
    pub target_sw: Component<'a>,
    pub target_hw: Component<'a>,
    pub other: Component<'a>,
}

impl<'a> Wfn<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn builder() -> CpeBuilder<'a, Wfn<'a>> {
        CpeBuilder::default()
    }

    pub fn set_part(&mut self, part: &'a str) -> Result<()> {
        self.part = CpeType::try_from(part)?;
        Ok(())
    }

    pub fn set_vendor(&mut self, vendor: &'a str) -> Result<()> {
        self.vendor = Component::parse_wfn_field(vendor)?;
        Ok(())
    }

    pub fn set_product(&mut self, product: &'a str) -> Result<()> {
        self.product = Component::parse_wfn_field(product)?;
        Ok(())
    }

    pub fn set_version(&mut self, version: &'a str) -> Result<()> {
        self.version = Component::parse_wfn_field(version)?;
        Ok(())
    }

    pub fn set_update(&mut self, update: &'a str) -> Result<()> {
        self.update = Component::parse_wfn_field(update)?;
        Ok(())
    }

    pub fn set_edition(&mut self, edition: &'a str) -> Result<()> {
        self.edition = Component::parse_wfn_field(edition)?;
        Ok(())
    }

    pub fn set_language(&mut self, language: &'a str) -> Result<()> {
        self.language = if language == "ANY" { Language::Any } else { Language::Language(language.parse()?) };
        Ok(())
    }

    pub fn set_sw_edition(&mut self, sw_edition: &'a str) -> Result<()> {
        self.sw_edition = Component::parse_wfn_field(sw_edition)?;
        Ok(())
    }

    pub fn set_target_sw(&mut self, target_sw: &'a str) -> Result<()> {
        self.target_sw = Component::parse_wfn_field(target_sw)?;
        Ok(())
    }

    pub fn set_target_hw(&mut self, target_hw: &'a str) -> Result<()> {
        self.target_hw = Component::parse_wfn_field(target_hw)?;
        Ok(())
    }

    pub fn set_other(&mut self, other: &'a str) -> Result<()> {
        self.other = Component::parse_wfn_field(other)?;
        Ok(())
    }

    pub fn parse(value: &'a str) -> Result<Self> {
        let mut offset = if value.starts_with("wfn:[") {
            5
        } else {
            return Err(CpeError::InvalidPrefix {
                value: value.to_owned(),
            });
        };

        let mut wfn = Self::new();

        let mut remainder = &value[offset..];

        macro_rules! take_quote {
            () => {{
                if remainder.chars().next() == Some('"') {
                    offset += 1;
                    remainder = &remainder[1..];
                    true
                } else {
                    false
                }
            }};
        }

        macro_rules! parse_component {
            ($field:ident) => {{
                eprintln!("parsing {:?} : \"{}\"", stringify!($field), remainder);
                let ($field, new_offset) =
                    ComponentStringParser::<Wfn>::parse_offset_value(value, offset)?;
                offset = new_offset;
                eprintln!("  parsed : {:?}", $field);
                eprintln!("  new_offset : {}", new_offset);
                eprintln!("  offset: {}, {:?}", offset, &value[offset..]);
                wfn.$field = $field;
                remainder = &value[new_offset..];
                eprintln!("  remainder : {:?}", remainder);
            }};
        }

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

        macro_rules! check_dupe {
            ($name:literal, $check:ident) => {
                if $check {
                    return Err(CpeError::DuplicateAttribute { value: value.to_owned(), name: $name });
                } else {
                    $check = true;
                }
            }
        }

        while !remainder.is_empty() {
            let (name, r) = {
                let mut parts = remainder.splitn(2, '=');
                (
                    parts.next().ok_or_else(|| CpeError::InvalidWfn {
                        value: value.to_owned(),
                        expected: "expected another pair".to_owned(),
                    })?.to_lowercase(),
                    parts.next().ok_or_else(|| CpeError::InvalidWfn {
                        value: value.to_owned(),
                        expected: "expected another pair".to_owned(),
                    })?,
                )
            };
            remainder = r;
            offset += name.len() + 1;
            eprintln!("offset: {}, {:?}", offset, &value[offset..]);
            let took_quote = take_quote!();
            dbg!(remainder);
            eprintln!("offset: {}, {:?}", offset, &value[offset..]);
            match name.as_str() {
                "part" => {
                    check_dupe!("part", set_part);
                    let (part, r) = remainder.split_at(1);
                    offset += part.len();
                    eprintln!("offset: {}, {:?}", offset, &value[offset..]);
                    remainder = r;
                    dbg!(part);
                    dbg!(remainder);
                    wfn.set_part(part)?;
                    dbg!(remainder);
                }
                "language" => {
                    check_dupe!("language", set_language);
                    if remainder.starts_with("ANY") {
                        offset += 3;
                        remainder = &remainder[3..];
                        wfn.language = Language::Any;
                    } else {
                        let (lang, r) = {
                            let mut parts = remainder.splitn(2, '"');
                            (
                                parts.next().ok_or_else(|| CpeError::InvalidWfn {
                                    value: value.to_owned(),
                                    expected: "expected closing `\"`".to_owned(),
                                })?,
                                parts.next().ok_or_else(|| CpeError::InvalidWfn {
                                    value: value.to_owned(),
                                    expected: "expected closing `\"`".to_owned(),
                                })?,
                            )
                        };
                        remainder = r;
                        wfn.set_language(lang)?;
                    }
                }
                "vendor" => {
                    check_dupe!("vendor", set_vendor);
                    parse_component!(vendor)
                }
                "product" => {
                    check_dupe!("product", set_product);
                    parse_component!(product)
                }
                "version" => {
                    check_dupe!("version", set_version);
                    parse_component!(version)
                }
                "update" => {
                    check_dupe!("update", set_update);
                    parse_component!(update)
                }
                "edition" => {
                    check_dupe!("edition", set_edition);
                    parse_component!(edition)
                }
                "sw_edition" => {
                    check_dupe!("sw_edition", set_sw_edition);
                    parse_component!(sw_edition)
                }
                "target_sw" => {
                    check_dupe!("target_sw", set_target_sw);
                    parse_component!(target_sw)
                }
                "target_hw" => {
                    check_dupe!("target_hw", set_target_hw);
                    parse_component!(target_hw)
                }
                "other" => {
                    check_dupe!("other", set_other);
                    parse_component!(other)
                }
                _ => {
                    return Err(CpeError::InvalidWfn {
                        value: value.to_owned(),
                        expected: format!("invalid attribute `{}`", name),
                    })
                }
            }
            if took_quote && !take_quote!() {
                return Err(CpeError::InvalidWfn {
                    value: value.to_owned(),
                    expected: "expected closing `\"`".to_owned(),
                });
            }
            eprintln!("  remainder took quote : {:?}", remainder);

            let c = remainder.chars().next();
            if c == Some(',') {
                offset += 1;
                remainder = &remainder[1..];
            } else if c == Some(']') {
                offset += 1;
                remainder = &remainder[1..];
                if !remainder.is_empty() {
                    return Err(CpeError::InvalidWfn {
                        value: value.to_owned(),
                        expected: format!("unexpected data after `]`: {}", remainder),
                    });
                }
            }
        }

        Ok(wfn)
    }
}

pub struct OwnedWfn {
    pub part: Option<CpeType>,
    pub vendor: Option<OwnedComponent>,
    pub product: Option<OwnedComponent>,
    pub version: Option<OwnedComponent>,
    pub update: Option<OwnedComponent>,
    pub edition: Option<OwnedComponent>,
    /// The language tag, in RFC 5646 (aka BCP47) format, as documented [here](https://tools.ietf.org/html/bcp47).
    /// Note that `cpe-rs` currently uses this for CPE v2.2 and CPE v2.3, despite CPE v2.2 using RFC 4646
    /// language tags, which were obsoleted by RFC 5646.
    pub language: Option<LanguageTag>,
    pub sw_edition: Option<OwnedComponent>,
    pub target_sw: Option<OwnedComponent>,
    pub target_hw: Option<OwnedComponent>,
    pub other: Option<OwnedComponent>,
}

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
            wfn!{
                part: "a",
                vendor: "microsoft",
                product: "internet_explorer",
                version: r"8\.*",
                update: "sp?",
                edition: "NA",
            }.unwrap()
        );
        assert_eq!(
            wfn,
            wfn!{
                part: "a",
                vendor: "microsoft",
                product: "internet_explorer",
                version: r"8\.*",
                update: "sp?",
                edition: "NA",
                language: "ANY",
            }.unwrap()
        )
    }
}
