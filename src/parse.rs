use crate::component::{Component, PackedComponents};
use crate::error::{CpeError, Result};
use crate::uri::Uri;
use crate::wfn::Wfn;

use std::iter::Peekable;
use std::marker::PhantomData;
use std::ops::{Bound, RangeBounds};
use std::str::Chars;

use std::borrow::Cow;
use std::mem;

pub struct ComponentStringParser<'a, T> {
    component: &'a str,
    position: usize,
    chars: Peekable<Chars<'a>>,
    current_value: Cow<'a, str>,
    remainder: &'a str,
    offset: Option<usize>,
    _ty: PhantomData<T>,
}

impl<'a, T> ComponentStringParser<'a, T> {
    pub fn new(component: &'a str) -> Self {
        Self {
            component,
            position: 0,
            chars: component.chars().peekable(),
            current_value: Cow::Borrowed(&component[0..0]),
            remainder: &component,
            offset: None,
            _ty: PhantomData,
        }
    }

    pub fn new_offset(component: &'a str, offset: usize) -> Result<Self> {
        if let Some(slice) = component.get(offset..) {
            Ok(Self {
                component,
                position: offset,
                chars: slice.chars().peekable(),
                current_value: Cow::Borrowed(&component[0..0]),
                remainder: &component,
                offset: Some(offset),
                _ty: PhantomData,
            })
        } else {
            Err(CpeError::InvalidOffset {
                value: component.to_owned(),
                offset,
            })
        }
    }

    #[inline]
    fn reset(&mut self) {
        self.position = self.offset.unwrap_or(0);
        self.current_value = Cow::Borrowed(&self.component_slice(..0));
        self.chars = self.component().chars().peekable();
    }

    #[inline]
    fn component(&self) -> &'a str {
        let start = self.offset.unwrap_or(0);
        &self.component[start..]
    }

    #[inline]
    fn component_slice<R: RangeBounds<usize>>(&self, range: R) -> &'a str {
        let offset = self.offset.unwrap_or(0);
        let start = match range.start_bound() {
            Bound::Included(i) => i + offset,
            Bound::Excluded(i) => i + offset + 1,
            Bound::Unbounded => offset,
        };
        match range.end_bound() {
            Bound::Included(i) => &self.component[start..=*i],
            Bound::Excluded(i) if *i == 0 => &self.component[start..start],
            Bound::Excluded(i) => &self.component[start..*i],
            Bound::Unbounded => &self.component[start..],
        }
    }

    #[inline]
    fn invalid_escape(&self) -> CpeError {
        CpeError::InvalidEscape {
            value: self.component.to_owned(),
            position: self.position + 1,
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn peek_expected(&mut self, expected: &str) -> Result<char> {
        self.chars
            .peek()
            .copied()
            .ok_or_else(|| CpeError::UnexpectedEnd {
                value: self.component.to_owned(),
                expected: expected.to_owned(),
            })
    }

    fn take(&mut self, number: usize, expected: &str) -> Result<()> {
        for _ in 0..number {
            self.chars.next().ok_or_else(|| CpeError::UnexpectedEnd {
                value: self.component.to_owned(),
                expected: expected.to_owned(),
            })?;
        }
        let new = self.position + number;

        self.current_value = match mem::take(&mut self.current_value) {
            Cow::Borrowed(_) => Cow::Borrowed(&self.component_slice(..new)),
            Cow::Owned(mut val) => {
                val.push_str(&self.component[self.position..new]);
                Cow::Owned(val)
            }
        };

        self.position = new;
        self.remainder = self
            .component
            .get(self.position..)
            .unwrap_or(&self.component_slice(..0));
        Ok(())
    }

    fn take_if<F>(&mut self, pred: F, expected: &str) -> Result<()>
    where
        F: Fn(char) -> bool,
    {
        let c = self.peek_expected(expected)?;

        if pred(c) {
            self.take(1, expected)
        } else {
            Err(CpeError::InvalidCharacter {
                value: self.component.to_owned(),
                position: self.position + 1,
                character: c,
                expected: expected.to_owned(),
            })
        }
    }
}

pub fn wfn_decode<'a, S: Into<Cow<'a, str>>>(value: S) -> Cow<'a, str> {
    let value = value.into();
    todo!()
}

impl<'a> ComponentStringParser<'a, Wfn<'a>> {
    fn take_decode(&mut self, number: usize, expected: &str) -> Result<()> {
        for _ in 0..number {
            self.chars.next().ok_or_else(|| CpeError::UnexpectedEnd {
                value: self.component.to_owned(),
                expected: expected.to_owned(),
            })?;
        }

        let new = self.position + number;
        let slice = &self.component[self.position..new];

        self.position = new;

        self.remainder = self
            .component
            .get(self.position..)
            .unwrap_or(&self.component_slice(..0));

        self.current_value = if slice.starts_with('\\') {
            match mem::take(&mut self.current_value) {
                Cow::Borrowed(val) => Cow::Owned(format!("{}{}", val, &slice[1..])),
                Cow::Owned(mut val) => {
                    val.push_str(&slice[1..]);
                    Cow::Owned(val)
                }
            }
        } else {
            match mem::take(&mut self.current_value) {
                Cow::Borrowed(_) => Cow::Borrowed(&self.component_slice(..new)),
                Cow::Owned(mut val) => {
                    val.push_str(slice);
                    Cow::Owned(val)
                }
            }
        };

        Ok(())
    }

    fn parse_special_body(&mut self) -> Result<()> {
        self.parse_spec_chars()?;
        self.parse_body2()?;
        while self.parse_body2().is_ok() {}
        Ok(())
    }

    fn parse_spec_chars(&mut self) -> Result<()> {
        match self.peek_expected("*, ?")? {
            '*' => self.take(1, "expected *")?,
            '?' => {
                self.take(1, "expected ?")?;
                while let Ok(()) = self.take_if(|c| c == '?', "expected ?") {}
            }
            c => {
                return Err(CpeError::InvalidCharacter {
                    value: self.component.to_owned(),
                    position: self.position + 1,
                    character: c,
                    expected: "expected *, ?".to_owned(),
                })
            }
        }
        Ok(())
    }

    fn parse_body2(&mut self) -> Result<()> {
        match self.peek_expected("ALPHA, DIGIT, _, espace")? {
            '\\' => self.parse_quoted2(),
            c if c.is_ascii_alphanumeric() || c == '_' => self.parse_unreserved(),
            c => Err(CpeError::InvalidCharacter {
                value: self.component.to_owned(),
                position: self.position + 1,
                character: c,
                expected: "expected ALPHA, DIGIT, _, escape".to_owned(),
            }),
        }
    }

    fn parse_quoted1(&mut self) -> Result<()> {
        let c = {
            let mut chars = self.remainder.chars();
            let esc = chars.next();
            if esc != Some('\\') {
                return Err(self.invalid_escape());
            }
            chars.next().ok_or_else(|| self.invalid_escape())?
        };

        if c.is_ascii_punctuation() {
            if c == '-' {
                Err(CpeError::InvalidCharacter {
                    value: self.component.to_owned(),
                    position: self.position + 1,
                    character: '-',
                    expected: "not allowed in this context".to_owned(),
                })
            } else {
                self.take_decode(2, "invalid escaped punctuation")
            }
        } else {
            Err(self.invalid_escape())
        }
    }

    fn parse_quoted2(&mut self) -> Result<()> {
        let c = {
            let mut chars = self.remainder.chars();
            let esc = chars.next();
            if esc != Some('\\') {
                return Err(self.invalid_escape());
            }
            chars.next().ok_or_else(|| self.invalid_escape())?
        };
        if c.is_ascii_punctuation() {
            self.take_decode(2, "invalid escaped punctuation")
        } else {
            Err(self.invalid_escape())
        }
    }

    fn parse_unreserved(&mut self) -> Result<()> {
        self.take_if(
            |c| c.is_ascii_alphanumeric() || c == '_',
            "expected alphanumeric or `_`",
        )
    }

    fn parse_body1(&mut self) -> Result<()> {
        match self.peek_expected("ALPHA, DIGIT, _, espace")? {
            '\\' => self.parse_quoted1(),
            c if c.is_ascii_alphanumeric() || c == '_' => self.parse_unreserved(),
            c => Err(CpeError::InvalidCharacter {
                value: self.component.to_owned(),
                position: self.position + 1,
                character: c,
                expected: "expected ALPHA, DIGIT, _, escape".to_owned(),
            }),
        }
    }

    fn parse_body(&mut self) -> Result<()> {
        if let Ok(()) = self.parse_body1() {
            while self.parse_body2().is_ok() {}
        } else {
            self.reset();
            self.parse_body2()?;
            self.parse_body2()?;
            while self.parse_body2().is_ok() {}
        }
        Ok(())
    }

    #[inline]
    pub fn parse_value(value: &'a str) -> Result<Component<'a>> {
        Self::new(value).parse()
    }

    #[inline]
    pub fn parse_offset_value(value: &'a str, offset: usize) -> Result<(Component<'a>, usize)> {
        Self::new_offset(value, offset)?.parse_offset()
    }

    pub fn parse_offset(&mut self) -> Result<(Component<'a>, usize)> {
        let component = self.parse()?;
        Ok((component, self.position))
    }

    pub fn parse(&mut self) -> Result<Component<'a>> {
        match self.peek_expected("?, *, ALPHA, DIGIT, _, escape")? {
            '?' | '*' => self.parse_special_body()?,
            _ => self.parse_body()?,
        }

        match self.peek() {
            Some('*') | Some('?') => self.parse_spec_chars()?,
            _ => (),
        }

        if self.current_value == "ANY" {
            Ok(Component::Any)
        } else if self.current_value == "NA" {
            Ok(Component::NotApplicable)
        } else {
            match self.chars.next() {
                Some('"') if self.offset.is_some() => {
                    Ok(Component::Value(mem::take(&mut self.current_value)))
                }
                None => Ok(Component::Value(mem::take(&mut self.current_value))),
                Some(c) => Err(CpeError::InvalidAVString {
                    value: self.component.to_owned(),
                    position: self.position + 1,
                    character: c,
                }),
            }
        }
    }
}

impl<'a> ComponentStringParser<'a, Uri<'a>> {
    fn take_decode(&mut self, number: usize, expected: &str) -> Result<()> {
        for _ in 0..number {
            self.chars.next().ok_or_else(|| CpeError::UnexpectedEnd {
                value: self.component.to_owned(),
                expected: expected.to_owned(),
            })?;
        }

        let new = self.position + number;
        let slice = &self.component[self.position..new];

        self.position = new;

        self.remainder = self
            .component
            .get(self.position..)
            .unwrap_or(&self.component_slice(..0));

        self.current_value = if slice == "%01" {
            Cow::Owned(format!("{}?", self.current_value))
        } else if slice == "%02" {
            Cow::Owned(format!("{}*", self.current_value))
        } else if slice.starts_with('%') {
            let val = u8::from_str_radix(&slice[1..], 16).map_err(|_| self.invalid_escape())?;
            let c = char::from(val);
            if c.is_ascii_alphanumeric() {
                Cow::Owned(format!("{}{}", self.current_value, c))
            } else {
                Cow::Owned(format!("{}\\{}", self.current_value, c))
            }
        } else {
            match mem::take(&mut self.current_value) {
                Cow::Borrowed(_) => Cow::Borrowed(&self.component_slice(..self.position)),
                Cow::Owned(mut val) => {
                    val.push_str(slice);
                    Cow::Owned(val)
                }
            }
        };

        Ok(())
    }

    fn eat_percent_encoded(&mut self) -> Result<()> {
        let (second, third) = {
            let mut chars = self.remainder.chars();
            let c = chars.next();
            if c != Some('%') {
                return Err(self.invalid_escape());
            }
            (
                chars.next().ok_or_else(|| self.invalid_escape())?,
                chars.next().ok_or_else(|| self.invalid_escape())?,
            )
        };

        if (second == '2' && matches!(third, '1'..='9' | 'a'..='f'))
            || (second == '3' && matches!(third, 'a'..='f'))
            || (matches!(second, '4' | '6') && third == '0')
            || (matches!(second, '5' | '7') && matches!(third, 'b'..='e'))
        {
            self.take_decode(3, "")
        } else {
            Err(self.invalid_escape())
        }
    }

    fn maybe_eat_special(&mut self) {
        if self.remainder.starts_with("%01") {
            while self.remainder.starts_with("%01") {
                self.take_decode(3, "")
                    .expect("failed to take after matching special percent encoding");
            }
        } else if self.remainder.starts_with("%02") {
            self.take_decode(3, "")
                .expect("failed to take after matching special percent encoding");
        }
    }

    fn eat_string(&mut self) -> Result<()> {
        self.maybe_eat_special();
        let is_unreserved = |c: char| c.is_ascii_alphanumeric() || matches!(c, '-' | '.' | '_');

        loop {
            match self.peek() {
                None => break,
                Some(c) if is_unreserved(c) => self.take(1, "unreserved character")?,
                Some('%')
                    if self.remainder.starts_with("%01") || self.remainder.starts_with("%02") =>
                {
                    break
                }
                Some('%') => self.eat_percent_encoded()?,
                Some('~') => return Ok(()), // skip the maybe_eat_special
                Some(':') if self.offset.is_some() => return Ok(()),
                Some(c) => {
                    return Err(CpeError::InvalidCharacter {
                        value: self.component.to_owned(),
                        position: self.position + 1,
                        character: c,
                        expected: "expected unreserved or percent escaped".to_owned(),
                    })
                }
            }
        }
        self.maybe_eat_special();
        Ok(())
    }

    fn _parse_packed(&mut self) -> Result<PackedComponents<'a>> {
        let mut parts = [
            Component::Any,
            Component::Any,
            Component::Any,
            Component::Any,
            Component::Any,
        ];

        for part in &mut parts {
            self.take_if(|c| c == '~', "expected `~`")?;
            self.eat_string()?;
            match mem::take(&mut self.current_value) {
                Cow::Borrowed(val) => *part = Component::new(&val[1..]),
                Cow::Owned(val) => *part = Component::new_string(val[1..].to_owned()),
            }
        }
        if let Some(c) = self.chars.next() {
            Err(CpeError::InvalidAVString {
                value: self.component.to_owned(),
                position: self.position + 1,
                character: c,
            })
        } else {
            Ok((
                mem::take(&mut parts[0]),
                mem::take(&mut parts[1]),
                mem::take(&mut parts[2]),
                mem::take(&mut parts[3]),
                mem::take(&mut parts[4]),
            ))
        }
    }

    pub fn parse_packed(&mut self) -> Result<PackedComponents<'a>> {
        if self.component_slice(..).starts_with('~') {
            Ok(self._parse_packed()?)
        } else {
            Ok((
                self.parse()?,
                Component::default(),
                Component::default(),
                Component::default(),
                Component::default(),
            ))
        }
    }

    #[inline]
    pub fn parse_packed_value(value: &'a str) -> Result<PackedComponents<'a>> {
        Self::new(value).parse_packed()
    }

    #[inline]
    pub fn parse_packed_offset(&mut self) -> Result<(PackedComponents<'a>, usize)> {
        let components = self.parse_packed()?;
        Ok((components, self.position))
    }

    #[inline]
    pub fn parsed_packed_offset_value(
        value: &'a str,
        offset: usize,
    ) -> Result<(PackedComponents<'a>, usize)> {
        Self::new_offset(value, offset)?.parse_packed_offset()
    }

    #[inline]
    pub fn parse_value(value: &'a str) -> Result<Component<'a>> {
        Self::new(value).parse()
    }

    #[inline]
    pub fn parse_offset_value(value: &'a str, offset: usize) -> Result<(Component<'a>, usize)> {
        Self::new_offset(value, offset)?.parse_offset()
    }

    pub fn parse_offset(&mut self) -> Result<(Component<'a>, usize)> {
        let component = self.parse()?;
        Ok((component, self.position))
    }

    /// Parse a CPE URI component
    ///
    /// Notes from [CPE23-N:6.1.2.1]:
    /// * the empty string in a URI component corresponds to the WFN value `ANY`.
    /// * a single hyphen "-" corresponds to the WFN value `NA`.
    pub fn parse(&mut self) -> Result<Component<'a>> {
        if self.component == "" {
            Ok(Component::Any)
        } else if self.component == "-" {
            Ok(Component::NotApplicable)
        } else {
            self.eat_string()?;
            match self.chars.next() {
                Some(':') if self.offset.is_some() => {
                    Ok(Component::Value(mem::take(&mut self.current_value)))
                }
                Some(c) => Err(CpeError::InvalidAVString {
                    value: self.component.to_owned(),
                    position: self.position + 1,
                    character: c,
                }),
                None => Ok(Component::Value(mem::take(&mut self.current_value))),
            }
        }
    }
}


use lazy_static::lazy_static;
use regex::Regex;

pub fn validate_wfn_attribute(value: &str) -> bool {
    value != "*" && WFN_REGEX.is_match(value)
}

pub fn parse_wfn_attribute<'a>(value: &'a str) -> Result<Component<'a>> {
    if value == "ANY" {
        Ok(Component::Any)
    } else if value == "NA" {
        Ok(Component::NotApplicable)
    } else if validate_wfn_attribute(&value) {
        let value = WFN_REPLACE.replace_all(value, "$esc");
        Ok(Component::Value(value))
    } else {
        todo!()
    }

}

pub fn validate_uri_attribute(value: &str) -> bool {
    URI_REGEX.is_match(value)
}

pub fn parse_uri_attribute<'a>(value: &'a str) -> Result<Component<'a>> {
    if value.is_empty() {
        Ok(Component::Any)
    } else if value == "-" {
        Ok(Component::NotApplicable)
    } else if validate_uri_attribute(value) {
        let value = if value.contains("%01") || value.contains("%02") {
            let value = value.replace("%01", "?").replace("%02", "*");
            let decoded = percent_encoding::percent_decode_str(&value).decode_utf8().map_err(|source| CpeError::Utf8Error { source, value: value.to_owned() })?;
            Cow::Owned((&*decoded).to_owned())
        } else {
            percent_encoding::percent_decode_str(value).decode_utf8().map_err(|source| CpeError::Utf8Error { source, value: value.to_owned() })?
        };
        Ok(Component::Value(value))
    } else {
        Err(CpeError::InvalidUri{ value: value.to_owned() })
    }
}

lazy_static!{
    static ref WFN_REGEX: Regex = {
        Regex::new(concat!(
            "^(?:",                                                                  // top group
                "(?:",                                                              // body group
                    "(?:",                                                          // body group 1
                        r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~?*])"##,        // body1
                        r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])*"##,     // body2
                    ")",                                                            // close body group 1
                    r##"|(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*]){2,}"##,     // or 2*body
                ")",                                                                // close body group
                r"|(?:(?:\?{1,}|\*)",                                                  // or spec_chrs
                r##"(?:\w|\\[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])*)"##,             // body2
            ")",                                                                    // close top group
            r"(?:\?{1,}|\*)?$",                                                      // optional special
        )).unwrap()
    };

    static ref WFN_REPLACE: Regex = Regex::new(r##"\\(?P<esc>[\\!"#$%&'()+,./:;<=>@\[\]^`{|}~\-?*])"##).unwrap();


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
        }
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

    macro_rules! test_ok_av {
        ($val:expr) => {
            let res = ComponentStringParser::<Wfn>::new($val).parse().unwrap();
            assert_eq!(res, Component::Value(Cow::Borrowed($val)))
        };
        ($val:expr, ANY) => {
            let res = ComponentStringParser::<Wfn>::new($val).parse().unwrap();
            assert_eq!(res, Component::Any)
        };
        ($val:expr, NA) => {
            let res = ComponentStringParser::<Wfn>::new($val).parse().unwrap();
            assert_eq!(res, Component::NotApplicable)
        };
        ($val:expr, $exp:literal) => {
            let res = ComponentStringParser::<Wfn>::new($val).parse().unwrap();
            assert_eq!(
                res,
                Component::Value(std::borrow::Cow::Owned($exp.to_owned()))
            )
        };
    }

    macro_rules! test_err_av {
        ($val:expr) => {
            assert!(ComponentStringParser::<Wfn>::new($val).parse().is_err())
        };
    }

    macro_rules! test_ok_uri {
        ($val:expr) => {
            let res = ComponentStringParser::<Uri>::new($val).parse().unwrap();
            assert_eq!(res, Component::Value(std::borrow::Cow::Borrowed($val)))
        };
        ($val:expr, ANY) => {
            let res = ComponentStringParser::<Uri>::new($val).parse().unwrap();
            assert_eq!(res, Component::Any)
        };
        ($val:expr, NA) => {
            let res = ComponentStringParser::<Uri>::new($val).parse().unwrap();
            assert_eq!(res, Component::NotApplicable)
        };
        ($val:expr, $exp:literal) => {
            let res = ComponentStringParser::<Uri>::new($val).parse().unwrap();
            assert_eq!(
                res,
                Component::Value(std::borrow::Cow::Owned($exp.to_owned()))
            )
        };
    }

    macro_rules! test_err_uri {
        ($val:expr) => {
            assert!(ComponentStringParser::<Uri>::new($val).parse().is_err())
        };
    }

    #[test]
    fn uri_success() {
        test_ok_uri!("normal");
        test_ok_uri!("normal.with.dot");
        test_ok_uri!("nor_ma_l");
        test_ok_uri!("-", NA);
        test_ok_uri!("", ANY);
        test_ok_uri!("normal%21", r"normal\!");
        test_ok_uri!("%01%01multiple", "??multiple");
        test_ok_uri!("multiple%01%01", "multiple??");
        test_ok_uri!("%01%01multiple%01%01", "??multiple??");
    }

    #[test]
    fn packed() {
        let res = ComponentStringParser::<Uri>::new("~a~b~c~d~e")
            .parse_packed()
            .unwrap();
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
        let res = ComponentStringParser::<Uri>::new("~~~~~")
            .parse_packed()
            .unwrap();
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
        assert!(ComponentStringParser::<Uri>::new("a~b~c~d~e")
            .parse_packed()
            .is_err());
        assert!(ComponentStringParser::<Uri>::new("~a~b~c~d~e~")
            .parse_packed()
            .is_err());
    }

    #[test]
    fn uri_failure() {
        test_err_uri!("invalid:colon");
        test_err_uri!("invalid%20percent");
        test_err_uri!("special.in%01.middle");
        test_err_uri!("%02%02multiple.spec2");
        test_err_uri!("multiple.spec2%02%02");
        test_err_uri!("%01%02spec1.spec2");
        test_err_uri!("spec1.spec2%01%02");
        test_err_uri!("%02%01spec2.spec1");
        test_err_uri!("spec2.spec1%02%01");
    }

    #[test]
    fn from_spec() {
        test_ok_av!(r"foo\-bar", "foo-bar");
        test_ok_av!("Acrobat_Reader");
        test_ok_av!(r#"\"oh_my\!\""#, r#""oh_my!""#);
        test_ok_av!(r#"g\+\+"#, "g++");
        test_ok_av!(r#"9\.?"#, "9.?");
        test_ok_av!("sr*");
        test_ok_av!(r"big\$money", "big$money");
        test_ok_av!(r"foo\:bar", "foo:bar");
        test_ok_av!(r"back\\slash_software", r"back\slash_software");
        test_ok_av!(r"with_quoted\~tilde", "with_quoted~tilde");
        test_ok_av!("*SOFT*");
        test_ok_av!(r"8\.??", "8.??");
        test_ok_av!(r"*8\.??", "*8.??");
        test_ok_av!("ANY", ANY);
        test_ok_av!("NA", NA);
    }


    #[test]
    fn av_offset() {
        let val = r#"12"foo\-bar", "#;
        let (res, end_pos) = ComponentStringParser::<Wfn>::parse_offset_value(val, 3).unwrap();
        assert_eq!(res, Component::Value(Cow::Borrowed("foo-bar")));
        assert_eq!(&val[end_pos..], "\", ");
    }

    #[test]
    fn uri_offset() {
        let val = r":foo:bar:foobar:-:";
        let (res, end_pos) = ComponentStringParser::<Uri>::parse_offset_value(val, 9).unwrap();
        assert_eq!(res, Component::Value(Cow::Borrowed("foobar")));
        assert_eq!(&val[end_pos..], ":-:");
    }

    #[test]
    fn failures() {
        test_err_av!("foo\"");
        test_err_av!(r"foo-bar");
        test_err_av!("Acrobat Reader");
        test_err_av!(r#""oh_my\!""#);
        test_err_av!(r#"\"oh_my!\""#);
        test_err_av!(r#"g++"#);
        test_err_av!(r#"9.?"#);
        test_err_av!(r"big$money");
        test_err_av!(r"foo:bar");
        test_err_av!(r"back\slack_software");
        test_err_av!(r"with_quoted~tidle");
        test_err_av!(r"8.??");
        test_err_av!(r"8\.?*");
        test_err_av!(r"8\.**");
        test_err_av!(r"8\.*?");
        test_err_av!("*");
        test_err_av!(r"\-");
        test_err_av!("?");
        test_err_av!("?*");
    }
}
