#![feature(test)]

extern crate test;

pub mod builder;
pub mod component;
pub mod cpe;
pub mod error;
pub mod parse;
pub mod uri;
pub mod wfn;

#[cfg(test)]
mod tests {
    use super::parse::*;
    use super::wfn::*;
    use super::uri::*;

    use test::Bencher;

    #[bench]
    fn bench_wfn_regex(b: &mut Bencher) {
        b.iter(|| parse_wfn_attribute(r"really\-long\~thing\?with_\+lot\'s_of\\escapes\!"));
    }

    #[bench]
    fn bench_wfn_parse(b: &mut Bencher) {
        b.iter(|| ComponentStringParser::<Wfn>::parse_value(r"really\-long\~thing\?with_\+lot\'s_of\\escapes\!"));
    }

    #[bench]
    fn bench_uri_regex(b: &mut Bencher) {
        b.iter(|| parse_uri_attribute(r"%01%01%01really%21long%22thing%23with_%24lot%25s_of%26escapes%27%02"))
    }

    #[bench]
    fn bench_uri_parse(b: &mut Bencher) {
        b.iter(|| ComponentStringParser::<Uri>::parse_value(r"%01%01%01really%21long%22thing%23with_%24lot%25s_of%26escapes%27%02"))
    }


}
