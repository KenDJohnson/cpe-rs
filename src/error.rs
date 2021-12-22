//! Error types and Result wrapper.
use thiserror::Error;

pub type Result<T> = std::result::Result<T, CpeError>;

#[derive(Error, Debug)]
pub enum CpeError {
    #[error("invalid uri `{value}`")]
    InvalidUri { value: String },
    #[error("invalid packed component `{value}` ({expected})")]
    InvalidPacked { value: String, expected: String },
    #[error("error decoding value `{value}`, not well formed UTF-8")]
    Utf8Error {
        #[source]
        source: std::str::Utf8Error,
        value: String,
    },
    #[error("invalid start offset `{offset:}` in value `{value}`")]
    InvalidOffset { value: String, offset: usize },
    #[error("invalid prefix for `{value}`")]
    InvalidPrefix { value: String },
    #[error("invalid character `{character}` at position {position} in `{value}` ({expected})")]
    InvalidCharacter {
        value: String,
        position: usize,
        character: char,
        expected: String,
    },
    #[error("invalid character `{character}` at position {position} in avstring `{value}`")]
    InvalidAVString {
        value: String,
        position: usize,
        character: char,
    },
    #[error("invalid character `{character}` at position {position} in component `{value}`")]
    InvalidComponent {
        value: String,
        position: usize,
        character: char,
    },
    #[error("invalid percent encoding at position {position} in `{value}`")]
    InvalidEscape { value: String, position: usize },
    #[error("CPE language error")]
    LanguageError {
        #[from]
        #[source]
        source: language_tags::ParseError,
    },
    #[error("Invalid CPE type \"{value}\"")]
    InvalidCpeType { value: String },
    #[error("invalid WFN `{value}` ({expected})")]
    InvalidWfn { value: String, expected: String },
    #[error("Unexpected end of input in `{value}` (expected {expected})")]
    UnexpectedEnd { value: String, expected: String },
    #[error("Duplicate `{name}` attirbute in `{value}`")]
    DuplicateAttribute { value: String, name: &'static str },
    #[error("Invalid attribute value `{value}`")]
    InvalidAttribute { value: String },
}
