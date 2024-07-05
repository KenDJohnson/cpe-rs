//! Builder structs for Uri and Wfn CPE values.

use crate::error::Result;
use crate::uri::Uri;
use crate::wfn::Wfn;

use std::marker::PhantomData;

/// Generic CPE builder to be used to create a `Wfn` or `Uri`.
///
/// Both `Wfn` and `Uri` have a `builder` method that can be used to construct
/// a builder rather than calling `CpeBuilder::default()`.
#[derive(Default)]
pub struct CpeBuilder<'a, T> {
    part: Option<&'a str>,
    vendor: Option<&'a str>,
    product: Option<&'a str>,
    version: Option<&'a str>,
    update: Option<&'a str>,
    edition: Option<&'a str>,
    language: Option<&'a str>,
    sw_edition: Option<&'a str>,
    target_sw: Option<&'a str>,
    target_hw: Option<&'a str>,
    other: Option<&'a str>,
    _ty: PhantomData<T>,
}

impl<'a, T> CpeBuilder<'a, T> {
    /// Set the CPE part type.
    pub fn part<'b>(&'b mut self, part: &'a str) -> &'b mut Self {
        self.part = Some(part);
        self
    }

    /// Set the CPE vendor.
    pub fn vendor<'b>(&'b mut self, vendor: &'a str) -> &'b mut Self {
        self.vendor = Some(vendor);
        self
    }

    /// Set the CPE product.
    pub fn product<'b>(&'b mut self, product: &'a str) -> &'b mut Self {
        self.product = Some(product);
        self
    }

    /// Set the CPE version
    pub fn version<'b>(&'b mut self, version: &'a str) -> &'b mut Self {
        self.version = Some(version);
        self
    }

    /// Set the CPE update.
    pub fn update<'b>(&'b mut self, update: &'a str) -> &'b mut Self {
        self.update = Some(update);
        self
    }

    /// Set the CPE edition.
    ///
    /// *Note:* when calling `validate` on a `CpeBuilder<'_, Uri>` the edition
    /// will be parsed as a packed string if `~` characters are present. For a
    /// `CpeBuilder<'_, Wfn>` the edition is parsed as normal.
    pub fn edition<'b>(&'b mut self, edition: &'a str) -> &'b mut Self {
        self.edition = Some(edition);
        self
    }

    /// Set the CPE language.
    pub fn language<'b>(&'b mut self, language: &'a str) -> &'b mut Self {
        self.language = Some(language);
        self
    }
}

impl<'a> CpeBuilder<'a, Wfn<'a>> {
    /// Parse the strings set in the builder as Wfn attribute values, validating
    /// their contents.
    pub fn validate(&self) -> Result<Wfn<'a>> {
        let mut wfn = Wfn::new();
        macro_rules! add_field {
            ($field:ident, $setter:ident) => {
                if let Some($field) = self.$field {
                    wfn.$setter($field)?;
                }
            };
        }
        add_field!(part, set_part);
        add_field!(vendor, set_vendor);
        add_field!(product, set_product);
        add_field!(version, set_version);
        add_field!(update, set_update);
        add_field!(language, set_language);
        add_field!(edition, set_edition);
        add_field!(sw_edition, set_sw_edition);
        add_field!(target_sw, set_target_sw);
        add_field!(target_hw, set_target_hw);
        add_field!(other, set_other);

        Ok(wfn)
    }

    /// Set the software edition.
    pub fn sw_edition<'b>(&'b mut self, sw_edition: &'a str) -> &'b mut Self {
        self.sw_edition = Some(sw_edition);
        self
    }

    /// Set the target software.
    pub fn target_sw<'b>(&'b mut self, target_sw: &'a str) -> &'b mut Self {
        self.target_sw = Some(target_sw);
        self
    }

    /// Set the target hardware attribute.
    pub fn target_hw<'b>(&'b mut self, target_hw: &'a str) -> &'b mut Self {
        self.target_hw = Some(target_hw);
        self
    }

    /// Set the other attribute.
    pub fn other<'b>(&'b mut self, other: &'a str) -> &'b mut Self {
        self.other = Some(other);
        self
    }
}

impl<'a> CpeBuilder<'a, Uri<'a>> {
    /// Parse the strings set in the builder as URI attribute values, validating
    /// their contents, and unpacking `edition` as relevant.
    pub fn validate(&self) -> Result<Uri<'a>> {
        let mut uri = Uri::new();
        macro_rules! add_field {
            ($field:ident, $setter:ident) => {
                if let Some($field) = self.$field {
                    uri.$setter($field)?;
                }
            };
        }
        add_field!(part, set_part);
        add_field!(vendor, set_vendor);
        add_field!(product, set_product);
        add_field!(version, set_version);
        add_field!(update, set_update);
        add_field!(edition, set_edition);
        add_field!(language, set_language);

        Ok(uri)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_builder() {
        let mut cpe = Uri::builder();
        cpe.part("a")
            .vendor("acme")
            .product("tools")
            .version("1.0.0")
            .update("u1")
            .edition("e1")
            .language("en");

        assert_eq!(
            cpe.validate().unwrap().to_string(),
            "cpe:/a:acme:tools:1.0.0:u1:e1:en"
        );
    }
}
