//! A Rust crate for working with CPE 2.3 Well Formed Named and CPE 2.2 URI strings.
//!
//! Request the `permissive_encoding` feature to allow literal `!`, `+`, and `\` characters in CPE
//! URIs. This is a violation of the CPE 2.2 specification, but provides some clemency for
//! developers while still flagging the most egregious errors.
pub mod builder;
pub mod component;
pub mod cpe;
pub mod error;
pub mod parse;
pub mod uri;
pub mod wfn;
