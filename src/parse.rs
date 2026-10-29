//! Parsing functions for Cpe and Wfn values
//!
//! This is exposed publicly for convenience's sake, but generally should not be needed,
//! intead using the [Uri::parse](../uri/struct.Uri.html#method.parse) and
//! [Wfn::parse](../wfn/struct.Wfn.html#method.parse) functions.
use crate::component::{Component, PackedComponents};
use crate::error::{CpeError, Result};

use std::borrow::Cow;
use std::sync::LazyLock;

use regex::Regex;

pub fn validate_wfn_attribute(value: &str) -> bool {
    value != "*" && WFN_REGEX.is_match(value)
}

pub fn parse_wfn_attribute(value: &str) -> Result<Component<'_>> {
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

fn ends_with_escaped_special_chars(value: &str) -> bool {
    let mut characters = value.chars().rev();
    if !matches!(characters.next(), Some('*' | '?')) {
        return false;
    }

    let mut escape_chars = 0;
    for c in characters {
        if c == '\\' {
            escape_chars += 1;
        } else {
            break;
        }
    }
    escape_chars % 2 != 0
}

fn has_escaped_special_chars_at_boundary(value: &str) -> bool {
    [r"\*", r"\?"]
        .iter()
        .any(|wildcard| value.starts_with(wildcard))
        || ends_with_escaped_special_chars(value)
}

/// Validates an attribute in an FSB
///
/// # Errors
/// 
/// * Returns [CpeError::InvalidAttribute] if invalid or unescaped characters are used
/// * Returns [CpeError::UnsupportedCircumfixSpecialCharacters] if the string starts or ends with
///   '\?' or '\*', as the library currently does not support preserving that escaping.
///
pub fn validate_fsb_attribute(value: &str) -> Result<()> {
    if !FSB_REGEX.is_match(value) {
        Err(CpeError::InvalidAttribute {
            value: value.to_owned(),
        })
    } else if has_escaped_special_chars_at_boundary(value) {
        Err(CpeError::UnsupportedCircumfixSpecialCharacters {
            value: value.to_string(),
        })
    } else {
        Ok(())
    }
}

static FSB_LANGUAGE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:[A-Za-z]{2,3})(?:-(?:[A-Za-z]{2}|[0-9]{3}))?$").unwrap());

/// Validates the language attribute within an FSB against the specification's regex
pub fn validate_fsb_language(value: &str) -> bool {
    FSB_LANGUAGE_REGEX.is_match(value) || value == "*"
}

#[cfg(not(feature = "permissive_encoding"))]
pub fn validate_uri_attribute(value: &str) -> bool {
    URI_REGEX.is_match(value)
}

#[cfg(feature = "permissive_encoding")]
pub fn validate_uri_attribute(value: &str) -> bool {
    PERMISSIVE_URI_REGEX.is_match(value)
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

pub fn parse_uri_attribute(value: &str) -> Result<Component<'_>> {
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
            Cow::Owned((*decoded).to_owned())
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

pub fn parse_packed_uri_attribute(value: &str) -> Result<PackedComponents<'_>> {
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

static WFN_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(concat!(
        "^(?:",                                                     // top group
        "(?:",                                                      // body group
        "(?:",                                                      // body group 1
        r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~?*])"##,        // body1
        r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])*"##,     // body2
        ")",                                                        // close body group 1
        r##"|(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*]){2,}"##, // or 2*body
        ")",                                                        // close body group
        r"|(?:(?:\?{1,}|\*)",                                       // or spec_chrs
        r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])*)"##,    // body2
        ")",                                                        // close top group
        r"(?:\?{1,}|\*)?$",                                         // optional special
    ))
    .unwrap()
});

static WFN_REPLACE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r##"\\(?P<esc>[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])"##).unwrap());
static WFN_ENCODE_REPLACE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r##"(?P<esc>[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])"##).unwrap());

#[cfg(feature = "permissive_encoding")]
static PERMISSIVE_URI_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(?:(?:%01)+|%02)?(?:[\w\-._+]|(:?\\?(?:\+|!))?|%(?:2[1-9a-f]|3[a-f]|[46]0|[57][b-e]))*(?:(?:%01)+|%02)?$").unwrap()
});
static URI_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"^(?:(?:%01)+|%02)?(?:[\w\-._+]|(:?\\?(?:\+|!))?|%(?:2[1-9a-f]|3[a-f]|[46]0|[57][b-e]))*(?:(?:%01)+|%02)?$"
    )
    .unwrap()
});

static FSB_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r##"^(?:(?:(?:\?)+|\*)?(?:[A-Za-z0-9_.-]|\\[\\?*!"#$%&'()+,/:;<=>@\[\]^`{|}~])+(?:(?:\?)+|\*)?|[\*-])$"##).unwrap()
});
static FSB_QUOTED_CHARACTER_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r##"\\(?P<esc>[\\?*!"#$%&'()+,/:;<=>@\[\]^`{|}~])"##).unwrap());
static FSB_ENCODE_CHARACTER_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r##"(?P<esc>[\\?*!"#$%&'()+,/:;<=>@\[\]^`{|}~])"##).unwrap());
static FSB_LEADING_WILDCARD_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?P<wildcards>(?:\\\?)+|\\\*)").unwrap());
static FSB_TRAILING_WILDCARD_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?P<wildcards>(?:\\\?)+|\\\*)$").unwrap());

fn encode_fsb_value<'a>(value: &Cow<'a, str>) -> Cow<'a, str> {
    let value = value.as_ref();
    let encoded = FSB_ENCODE_CHARACTER_REGEX.replace_all(value, r"\$esc");
    let encoded = FSB_LEADING_WILDCARD_REGEX
        .replace_all(&encoded, |captures: &regex::Captures<'_>| {
            captures["wildcards"].replace('\\', "")
        });

    let encoded = FSB_TRAILING_WILDCARD_REGEX
        .replace_all(&encoded, |captures: &regex::Captures<'_>| {
            captures["wildcards"].replace('\\', "")
        });
    Cow::Owned(encoded.into_owned())
}

/// Encodes an FSB attribute according to the specification, leading and trailing special characters
/// are preserved unencoded, all others are escaped using '\'.
pub fn encode_fsb_attribute<'a>(value: &'a Component<'a>) -> Cow<'a, str> {
    match value {
        Component::Any => Cow::Borrowed("*"),
        Component::NotApplicable => Cow::Borrowed("-"),
        Component::Value(val) => encode_fsb_value(val),
    }
}

/// Parses an FSB attribute, escaped characters are unescaped.
pub fn parse_fsb_attribute(value: &str) -> Result<Component<'_>> {
    if value == "*" {
        Ok(Component::Any)
    } else if value == "-" {
        Ok(Component::NotApplicable)
    } else {
        validate_fsb_attribute(value)?;
        Ok(Component::Value(
            FSB_QUOTED_CHARACTER_REGEX.replace_all(value, "$esc"),
        ))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! ok_wfn_av_re {
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

    macro_rules! err_wfn_av_re {
        ($val:expr) => {
            assert!(!validate_wfn_attribute($val));
        };
    }

    #[test]
    fn wfn_re() {
        ok_wfn_av_re!(r"foo\-bar", "foo-bar");
        ok_wfn_av_re!("Acrobat_Reader");
        ok_wfn_av_re!(r#"\"oh_my\!\""#, r#""oh_my!""#);
        ok_wfn_av_re!(r#"g\+\+"#, "g++");
        ok_wfn_av_re!(r#"9\.?"#, "9.?");
        ok_wfn_av_re!("sr*");
        ok_wfn_av_re!(r"big\$money", "big$money");
        ok_wfn_av_re!(r"foo\:bar", "foo:bar");
        ok_wfn_av_re!(r"back\\slash_software", r"back\slash_software");
        ok_wfn_av_re!(r"with_quoted\~tilde", "with_quoted~tilde");
        ok_wfn_av_re!("*SOFT*");
        ok_wfn_av_re!(r"8\.??", "8.??");
        ok_wfn_av_re!(r"*8\.??", "*8.??");
        ok_wfn_av_re!("ANY", ANY);
        ok_wfn_av_re!("NA", NA);
    }

    #[test]
    fn wfn_re_failures() {
        err_wfn_av_re!("foo\"");
        err_wfn_av_re!(r"foo-bar");
        err_wfn_av_re!("Acrobat Reader");
        err_wfn_av_re!(r#""oh_my\!""#);
        err_wfn_av_re!(r#"\"oh_my!\""#);
        err_wfn_av_re!(r#"g++"#);
        err_wfn_av_re!(r#"9.?"#);
        err_wfn_av_re!(r"big$money");
        err_wfn_av_re!(r"foo:bar");
        err_wfn_av_re!(r"back\slack_software");
        err_wfn_av_re!(r"with_quoted~tidle");
        err_wfn_av_re!(r"8.??");
        err_wfn_av_re!(r"8\.?*");
        err_wfn_av_re!(r"8\.**");
        err_wfn_av_re!(r"8\.*?");
        err_wfn_av_re!("*");
        err_wfn_av_re!(r"\-");
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

    macro_rules! ok_fsb_av_re_parse {
        ($val:expr) => {
            let res = parse_fsb_attribute($val).unwrap();
            assert_eq!(res, Component::Value(Cow::Borrowed($val)))
        };
        ($val:expr, ANY) => {
            let res = parse_fsb_attribute($val).unwrap();
            assert_eq!(res, Component::Any)
        };
        ($val:expr, NA) => {
            let res = parse_fsb_attribute($val).unwrap();
            assert_eq!(res, Component::NotApplicable)
        };
        ($val:expr, $exp:literal) => {
            let res = parse_fsb_attribute($val).unwrap();
            assert_eq!(
                res,
                Component::Value(std::borrow::Cow::Owned($exp.to_owned()))
            )
        };
    }

    macro_rules! ok_fsb_av_re_encode {
        ($val:expr) => {
            let res = encode_fsb_attribute(&Component::Value(Cow::Borrowed($val)));
            assert_eq!(res, $val)
        };
        (ANY, $val:expr) => {
            let res = encode_fsb_attribute(&Component::Any);
            assert_eq!(res, $val)
        };
        (NA, $val:expr) => {
            let res = encode_fsb_attribute(&Component::NotApplicable);
            assert_eq!(res, $val)
        };
        ($val:expr, $exp:literal) => {
            let res = encode_fsb_attribute(&Component::Value(Cow::Borrowed($val)));
            assert_eq!(res, $exp)
        };
    }

    macro_rules! err_fsb_av_re {
        ($val:expr) => {
            assert!(validate_fsb_attribute($val).is_err());
        };
    }

    #[test]
    fn fsb_re_parse() {
        ok_fsb_av_re_parse!("foo-bar");
        ok_fsb_av_re_parse!("Acrobat_Reader");
        ok_fsb_av_re_parse!(r#"\"oh_my\!\""#, r#""oh_my!""#);
        ok_fsb_av_re_parse!(r"g\+\+", r"g++");
        ok_fsb_av_re_parse!(r"9.?", "9.?");
        ok_fsb_av_re_parse!(r"big\$money", r"big$money");
        ok_fsb_av_re_parse!(r"foo\:bar", r"foo:bar");
        ok_fsb_av_re_parse!(r"back\\slash_software", r"back\slash_software");
        ok_fsb_av_re_parse!(r"foo\\bar", r"foo\bar");
        ok_fsb_av_re_parse!(r"with_quoted\~tilde", r"with_quoted~tilde");
        ok_fsb_av_re_parse!(r"*SOFT*");
        ok_fsb_av_re_parse!(r"8.??");
        ok_fsb_av_re_parse!(r"8.\\?", r"8.\?");
        ok_fsb_av_re_parse!(r"8.\\\\?", r"8.\\?");
        ok_fsb_av_re_parse!("*", ANY);
        ok_fsb_av_re_parse!("-", NA);
        ok_fsb_av_re_parse!(r"\\\*foo", r"\*foo");
    }

    #[test]
    fn fsb_re_encode() {
        ok_fsb_av_re_encode!("foo-bar");
        ok_fsb_av_re_encode!("Acrobat_Reader");
        ok_fsb_av_re_encode!(r#""oh_my!""#, r#"\"oh_my\!\""#);
        ok_fsb_av_re_encode!(r"g++", r"g\+\+");
        ok_fsb_av_re_encode!("9.?", r"9.?");
        ok_fsb_av_re_encode!("sr*");
        ok_fsb_av_re_encode!(r"big$money", r"big\$money");
        ok_fsb_av_re_encode!(r"foo:bar", r"foo\:bar");
        ok_fsb_av_re_encode!(r"back\slash_software", r"back\\slash_software");
        ok_fsb_av_re_encode!(r"foo\bar", r"foo\\bar");
        ok_fsb_av_re_encode!(r"with_quoted~tilde", r"with_quoted\~tilde");
        ok_fsb_av_re_encode!(r"*SOFT*");
        ok_fsb_av_re_encode!(r"8.??");
        ok_fsb_av_re_encode!(ANY, "*");
        ok_fsb_av_re_encode!(NA, "-");
    }

    #[test]
    fn fsb_re_failures() {
        err_fsb_av_re!("Acrobat Reader");
        err_fsb_av_re!(r#""oh_my\!""#);
        err_fsb_av_re!(r#"\"oh_my!\""#);
        err_fsb_av_re!(r#"g++"#);
        err_fsb_av_re!(r"big$money");
        err_fsb_av_re!(r"foo:bar");
        err_fsb_av_re!(r"back\slack_software");
        err_fsb_av_re!(r"with_quoted~tidle");
        err_fsb_av_re!(r"8\.?*");
        err_fsb_av_re!(r"8\.**");
        err_fsb_av_re!(r"8\.*?");
        err_fsb_av_re!(r"\-");
        err_fsb_av_re!(r"sr\*");
        err_fsb_av_re!(r"8.\?\?");
        err_fsb_av_re!(r"\*sr");
        err_fsb_av_re!(r"\?sr");
    }
}
