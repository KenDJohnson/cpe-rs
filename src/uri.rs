//! # Uri - A CPE 2.2 URI
//!
//! CPE 2.2 URI as specified in [CPE22](https://cpe.mitre.org/files/cpe-specification_2.2.pdf) or in
//! [CPE23-N:6](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf)
//!
//! ## Grammar
//! The specification for a URI from [CPE23-N:6](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf)
//! defines both a strict grammar, and a more permissive grammar. The URIs are parsed with the permissive
//! grammar by default, but can be parsed with the strict grammar with TODO link.
//!
//! ### Strict Grammar
//!
//! ```ignore
//! cpe-name         = "cpe:/" component-list
//! component-list   = part ":" vendor ":" product ":" version ":" update ":" edition ":" lang
//! component-list   /= part ":" vendor ":" product ":" version ":" update ":" edition
//! component-list   /= part ":" vendor ":" product ":" version ":" update
//! component-list   /= part ":" vendor ":" product ":" version
//! component-list   /= part ":" vendor ":" product
//! component-list   /= part ":" vendor
//! component-list   /= part
//! component-list   /= empty
//! part             = "h" / "o" / "a" / empty
//! vendor           = string
//! product          = string
//! version          = string
//! update           = string
//! edition          = string
//! lang             = LANGTAG / empty
//! string           = *( unreserved / pct-encoded )
//! empty            = ""
//! unreserved       = ALPHA / DIGIT / "-" / "." / "_" / "˜"
//! pct-encoded      = "%" HEXDIG HEXDIG
//! ALPHA            = %x41-5a / %x61-7a ; A-Z or a-z
//! DIGIT            = %x30-39 ; 0-9
//! HEXDIG           = DIGIT / "a" / "b" / "c" / "d" / "e" / "f"
//! LANGTAG          = cf. [RFC5646]
//! ```
//!
//! ### Permissive Grammar
//!
//! ```ignore
//! cpe-name         = "cpe:/" component-list
//! component-list   = part ":" vendor ":" product ":" version ":" update ":" edition ":" lang
//! component-list   /= part ":" vendor ":" product ":" version ":" update ":" edition
//! component-list   /= part ":" vendor ":" product ":" version ":" update
//! component-list   /= part ":" vendor ":" product ":" version
//! component-list   /= part ":" vendor ":" product
//! component-list   /= part ":" vendor
//! component-list   /= part
//! component-list   /= empty
//! part             = "h" / "o" / "a" / empty
//! vendor           = string
//! product          = string
//! version          = string
//! update           = string
//! edition          = string / packed
//! lang             = LANGTAG / empty
//! string           = str_wo_special / str_w_special
//! str_wo_special   = *( unreserved / pct-encoded )
//! str_w_special    = *1spec_chrs 1*( unreserved / pct-encoded ) *1spec_chrs
//! spec_chrs        = 1*spec1 / spec2
//! packed           = "~" string "~" string "~" string "~" string "~" string
//! empty            = ""
//! unreserved       = ALPHA / DIGIT / "-" / "." / "_"
//! special          = spec1 / spec2
//! spec1            = "%01"
//! spec2            = "%02"
//! pct-encoded      = "%21" / "%22" / "%23" / "%24" / "%25" / "%26" / "%27" /
//!                  = "%28" / "%29" / "%2a" / "%2b" / "%2c" / "%2f" / "%3a" /
//!                  = "%3b" / "%3c" / "%3d" / "%3e" / "%3f" / "%40" / "%5b" /
//!                  = "%5c" / "%5d" / "%5e" / "%60" / "%7b" / "%7c" / "%7d" /
//!                  = "%7e"
//! ALPHA            = %x41-5a / %x61-7a ; A-Z or a-z
//! DIGIT            = %x30-39 ; 0-9
//! LANGTAG          = language ["-" region] ; cf. [RFC5646]
//! language         = 2*3ALPHA ; shortest ISO 639 code
//! region           = 2ALPHA / 3DIGIT ; ISO 3166-1 code or UN M.49 code
//! ```
//!
//! ### Prefixes
//!
//! Some vendors use a prefix to denote a CPE that is not in a CPE dictionary. This is a two character
//! prefix before the leading `cpe:/`, typically in the form of `x-cpe:/` or `p-cpe:/`. A URI in this form
//! can be parsed with the `parse_prefix` function. There is no function to parse prefixed URIs using the
//! strict grammar.

use crate::builder::CpeBuilder;
use crate::component::Component;
use crate::cpe::{CpeType, Language};
use crate::error::{CpeError, Result};
use crate::parse::ComponentStringParser;
use crate::wfn::Wfn;

use std::convert::TryFrom;

#[derive(Default, Debug, PartialEq)]
pub struct Uri<'a> {
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

impl<'a> Uri<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn builder() -> CpeBuilder<'a, Uri<'a>> {
        CpeBuilder::default()
    }

    pub fn set_part(&mut self, part: &'a str) -> Result<()> {
        self.part = CpeType::try_from(part)?;
        Ok(())
    }

    pub fn set_vendor(&mut self, vendor: &'a str) -> Result<()> {
        self.vendor = Component::parse_uri_field(vendor)?;
        Ok(())
    }

    pub fn set_product(&mut self, product: &'a str) -> Result<()> {
        self.product = Component::parse_uri_field(product)?;
        Ok(())
    }

    pub fn set_version(&mut self, version: &'a str) -> Result<()> {
        self.version = Component::parse_uri_field(version)?;
        Ok(())
    }

    pub fn set_update(&mut self, update: &'a str) -> Result<()> {
        self.update = Component::parse_uri_field(update)?;
        Ok(())
    }

    pub fn set_edition(&mut self, edition: &'a str) -> Result<()> {
        let (edition, sw_edition, target_sw, target_hw, other) =
            ComponentStringParser::<Uri>::parse_packed_value(edition)?;
        self.edition = edition;
        self.sw_edition = sw_edition;
        self.target_sw = target_sw;
        self.target_hw = target_hw;
        self.other = other;
        Ok(())
    }

    pub fn set_language(&mut self, language: &'a str) -> Result<()> {
        self.language = if language == "ANY" { Language::Any } else { Language::Language(language.parse()?) };
        Ok(())
    }

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
                    ComponentStringParser::<Uri>::new(c).parse()?
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
            .map(|edition| ComponentStringParser::<Uri>::parse_packed_value(edition))
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
