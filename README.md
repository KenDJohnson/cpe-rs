# CPE handling library in Rust

## Conformance

- [ ] An implementation MUST make an explicit claim of conformance to this specification in any
documentation provided to end users.
- [ ] If the implementation produces (i.e., generates as an output) CPE names, it MUST produce
syntactically correct formatted string bindings as needed to describe or identify applications,
operating systems, and hardware devices (cf. 6.2).
- [ ] If the implementation produces CPE names in URI form, it MUST produce URIs that adhere to
the syntax rules specified in Figure 6-1, and it SHOULD produce URIs that adhere to the more
constrained rules specified in Figure 6-2.
- [ ] If the implementation consumes (i.e., accepts as valid input) CPE names, (a) it MUST consume
syntactically correct formatted string bindings as needed to describe or identify applications,
operating systems, and hardware devices (cf. 6.2); (b) it SHOULD be able to consume any CPE
name that meets the syntax requirements specified in Figure 6-1 or Figure 6-2; and (c) it
SHOULD be able to convert input CPE names in URI form to syntactically correct formatted
string bindings (cf. 7.1).
- [ ] It is OPTIONAL for implementations to be able to convert any syntactically correct formatted
string binding to a valid CPE name that meets the requirements specified in [CPE22] (cf. 7.2). An
implementation that offers this functionality SHOULD implement the procedure defined in
Section 7.2. This functionality may enable an implementation to interoperate to a limited extent
with implementations conforming to [CPE22] and possibly prior releases as well.

Source: [\[CPE23-N\]](#\[2\])

As a special exception to section 5.4 of the CPE standard, the `permissive_encoding` feature will
permit URIs with `\`, `+`, and `!` characters. While not intended for long term operation, this
feature provides a method to find more egregious issues in CPE URIs while developers fix encoding
issues for these commonly used characters.

## References

###### \[1\]
[CPE Specification Overview](https://cpe.mitre.org/specification/)

###### \[2\]
[CPE Naming Specification [CPE23-N]](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7695.pdf)

###### \[3\]
[CPE Name Matching](https://nvlpubs.nist.gov/nistpubs/Legacy/IR/nistir7696.pdf)

###### \[4\]
[CPE Specification v2.2 [CPE-22]](https://cpe.mitre.org/files/cpe-specification_2.2.pdf)
