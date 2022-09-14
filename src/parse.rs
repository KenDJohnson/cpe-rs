//! Parsing functions for Cpe and Wfn values
//!
//! This is exposed publicly for convenience's sake, but generally should not be needed,
//! intead using the [Uri::parse](../uri/struct.Uri.html#method.parse) and
//! [Wfn::parse](../wfn/struct.Wfn.html#method.parse) functions.
use crate::component::{Component, PackedComponents};
use crate::error::{CpeError, Result};

use std::borrow::Cow;

use lazy_static::lazy_static;
use regex::Regex;

pub fn validate_wfn_attribute(value: &str) -> bool {
    value != "*" && WFN_REGEX.is_match(value)
}

pub fn parse_wfn_attribute(value: &str) -> Result<Component> {
    if value == "ANY" {
        Ok(Component::Any)
    } else if value == "NA" {
        Ok(Component::NotApplicable)
    } else if validate_wfn_attribute(value) {
        let value = WFN_REPLACE.replace_all(value, "$esc");
        Ok(Component::Value(value))
    } else {
        Err(CpeError::InvalidAttribute {
            value: value.to_owned(),
        })
    }
}

pub fn validate_uri_attribute(value: &str) -> bool {
    URI_REGEX.is_match(value)
}

pub fn encode_wfn_attribute<'a>(value: &'a Component<'a>) -> Cow<'a, str> {
    match value {
        Component::Any => Cow::Borrowed("ANY"),
        Component::NotApplicable => Cow::Borrowed("NA"),
        Component::Value(val) => WFN_ENCODE_REPLACE.replace_all(val, r"\$esc"),
    }
}

static RESERVED: percent_encoding::AsciiSet = percent_encoding::CONTROLS
    .add(b'!')
    .add(b'"')
    .add(b'#')
    .add(b'$')
    .add(b'%')
    .add(b'&')
    .add(b'\'')
    .add(b'(')
    .add(b')')
    .add(b'+')
    .add(b',')
    .add(b'.')
    .add(b'/')
    .add(b':')
    .add(b':')
    .add(b'<')
    .add(b'=')
    .add(b'>')
    .add(b'@')
    .add(b'[')
    .add(b']')
    .add(b'^')
    .add(b'`')
    .add(b'{')
    .add(b'|')
    .add(b'}')
    .add(b'~');

pub fn encode_uri_attribute<'a>(value: &'a Component<'a>) -> Cow<'a, str> {
    match value {
        Component::Any => Cow::Borrowed("*"),
        Component::NotApplicable => Cow::Borrowed("?"),
        Component::Value(val) => {
            if val.contains('?') || val.contains('*') {
                Cow::Owned(
                    percent_encoding::utf8_percent_encode(
                        &val.replace('?', "%01").replace('*', "%02"),
                        &RESERVED,
                    )
                    .to_string(),
                )
            } else {
                percent_encoding::utf8_percent_encode(val, &RESERVED).into()
            }
        }
    }
}

pub fn parse_uri_attribute(value: &str) -> Result<Component> {
    if value.is_empty() {
        Ok(Component::Any)
    } else if value == "-" {
        Ok(Component::NotApplicable)
    } else if validate_uri_attribute(value) {
        let value = if value.contains("%01") || value.contains("%02") {
            let value = value.replace("%01", "?").replace("%02", "*");
            let decoded = percent_encoding::percent_decode_str(&value)
                .decode_utf8()
                .map_err(|source| CpeError::Utf8Error {
                    source,
                    value: value.to_owned(),
                })?;
            Cow::Owned((&*decoded).to_owned())
        } else {
            percent_encoding::percent_decode_str(value)
                .decode_utf8()
                .map_err(|source| CpeError::Utf8Error {
                    source,
                    value: value.to_owned(),
                })?
        };
        Ok(Component::Value(value))
    } else {
        Err(CpeError::InvalidUri {
            value: value.to_owned(),
        })
    }
}

pub fn parse_packed_uri_attribute(value: &str) -> Result<PackedComponents> {
    if value.starts_with('~') {
        let parts = value.split('~').collect::<Vec<_>>();
        if parts.len() != 6 {
            return Err(CpeError::InvalidPacked {
                value: value.to_owned(),
                expected: format!("expected 5 packed items, got {}", parts.len() - 1),
            });
        }

        Ok((
            parse_uri_attribute(parts[1])?,
            parse_uri_attribute(parts[2])?,
            parse_uri_attribute(parts[3])?,
            parse_uri_attribute(parts[4])?,
            parse_uri_attribute(parts[5])?,
        ))
    } else {
        Ok((
            parse_uri_attribute(value)?,
            Component::Any,
            Component::Any,
            Component::Any,
            Component::Any,
        ))
    }
}

lazy_static! {
    static ref WFN_REGEX: Regex = {
        Regex::new(concat!(
            "^(?:",                                                                 // top group
                "(?:",                                                              // body group
                    "(?:",                                                          // body group 1
                        r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~?*])"##,        // body1
                        r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])*"##,     // body2
                    ")",                                                            // close body group 1
                    r##"|(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*]){2,}"##,     // or 2*body
                ")",                                                                // close body group
                r"|(?:(?:\?{1,}|\*)",                                               // or spec_chrs
                r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])*)"##,            // body2
            ")",                                                                    // close top group
            r"(?:\?{1,}|\*)?$",                                                     // optional special
        )).unwrap()
    };

    static ref WFN_REPLACE: Regex = Regex::new(r##"\\(?P<esc>[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])"##).unwrap();

    static ref WFN_ENCODE_REPLACE: Regex = Regex::new(r##"(?P<esc>[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])"##).unwrap();


    static ref URI_REGEX: Regex = Regex::new(concat!(
        r"^(?:(?:%01)+|%02)?(?:[\w\-._]|%(?:2[1-9a-f]|3[a-f]|[46]0|[57][b-e]))*(?:(?:%01)+|%02)?$"
    )).unwrap();
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! ok_av_re {
        ($val:expr) => {
            let res = parse_wfn_attribute($val).unwrap();
            assert_eq!(res, Component::Value(Cow::Borrowed($val)))
        };
        ($val:expr, ANY) => {
            let res = parse_wfn_attribute($val).unwrap();
            assert_eq!(res, Component::Any)
        };
        ($val:expr, NA) => {
            let res = parse_wfn_attribute($val).unwrap();
            assert_eq!(res, Component::NotApplicable)
        };
        ($val:expr, $exp:literal) => {
            let res = parse_wfn_attribute($val).unwrap();
            assert_eq!(
                res,
                Component::Value(std::borrow::Cow::Owned($exp.to_owned()))
            )
        };
    }

    macro_rules! err_av_re {
        ($val:expr) => {
            assert!(!validate_wfn_attribute($val));
        };
    }

    #[test]
    fn wfn_re() {
        ok_av_re!(r"foo\-bar", "foo-bar");
        ok_av_re!("Acrobat_Reader");
        ok_av_re!(r#"\"oh_my\!\""#, r#""oh_my!""#);
        ok_av_re!(r#"g\+\+"#, "g++");
        ok_av_re!(r#"9\.?"#, "9.?");
        ok_av_re!("sr*");
        ok_av_re!(r"big\$money", "big$money");
        ok_av_re!(r"foo\:bar", "foo:bar");
        ok_av_re!(r"back\\slash_software", r"back\slash_software");
        ok_av_re!(r"with_quoted\~tilde", "with_quoted~tilde");
        ok_av_re!("*SOFT*");
        ok_av_re!(r"8\.??", "8.??");
        ok_av_re!(r"*8\.??", "*8.??");
        ok_av_re!("ANY", ANY);
        ok_av_re!("NA", NA);
    }

    #[test]
    fn re_failures() {
        err_av_re!("foo\"");
        err_av_re!(r"foo-bar");
        err_av_re!("Acrobat Reader");
        err_av_re!(r#""oh_my\!""#);
        err_av_re!(r#"\"oh_my!\""#);
        err_av_re!(r#"g++"#);
        err_av_re!(r#"9.?"#);
        err_av_re!(r"big$money");
        err_av_re!(r"foo:bar");
        err_av_re!(r"back\slack_software");
        err_av_re!(r"with_quoted~tidle");
        err_av_re!(r"8.??");
        err_av_re!(r"8\.?*");
        err_av_re!(r"8\.**");
        err_av_re!(r"8\.*?");
        err_av_re!("*");
        err_av_re!(r"\-");
    }

    macro_rules! ok_uri_re {
        ($val:expr) => {
            let res = parse_uri_attribute($val).unwrap();
            assert_eq!(res, Component::Value(std::borrow::Cow::Borrowed($val)))
        };
        ($val:expr, ANY) => {
            let res = parse_uri_attribute($val).unwrap();
            assert_eq!(res, Component::Any)
        };
        ($val:expr, NA) => {
            let res = parse_uri_attribute($val).unwrap();
            assert_eq!(res, Component::NotApplicable)
        };
        ($val:expr, $exp:literal) => {
            let res = parse_uri_attribute($val).unwrap();
            assert_eq!(
                res,
                Component::Value(std::borrow::Cow::Owned($exp.to_owned()))
            )
        };
    }

    macro_rules! err_uri_re {
        ($val:expr) => {
            assert!(!validate_uri_attribute($val));
        };
    }

    #[test]
    fn uri_success_re() {
        ok_uri_re!("normal");
        ok_uri_re!("normal.with.dot");
        ok_uri_re!("nor_ma_l");
        ok_uri_re!("-", NA);
        ok_uri_re!("", ANY);
        ok_uri_re!("normal%21", r"normal!");
        ok_uri_re!("%01%01multiple", "??multiple");
        ok_uri_re!("multiple%01%01", "multiple??");
        ok_uri_re!("%01%01multiple%01%01", "??multiple??");
    }

    #[test]
    fn uri_failure_re() {
        err_uri_re!("invalid:colon");
        err_uri_re!("invalid%20percent");
        err_uri_re!("special.in%01.middle");
        err_uri_re!("%02%02multiple.spec2");
        err_uri_re!("multiple.spec2%02%02");
        err_uri_re!("%01%02spec1.spec2");
        err_uri_re!("spec1.spec2%01%02");
        err_uri_re!("%02%01spec2.spec1");
        err_uri_re!("spec2.spec1%02%01");
    }

    #[test]
    fn packed() {
        let res = parse_packed_uri_attribute("~a~b~c~d~e").unwrap();
        assert_eq!(
            res,
            (
                Component::new("a"),
                Component::new("b"),
                Component::new("c"),
                Component::new("d"),
                Component::new("e")
            )
        );
        let res = parse_packed_uri_attribute("~~~~~").unwrap();
        assert_eq!(
            res,
            (
                Component::Any,
                Component::Any,
                Component::Any,
                Component::Any,
                Component::Any,
            )
        );
        assert!(parse_packed_uri_attribute("a~b~c~d~e").is_err());
        assert!(parse_packed_uri_attribute("~a~b~c~d~e~").is_err());
    }
}
