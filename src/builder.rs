use crate::error::Result;
use crate::uri::Uri;
use crate::wfn::Wfn;

use std::marker::PhantomData;

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
    pub fn part<'b>(&'b mut self, part: &'a str) -> &'b mut Self {
        self.part = Some(part);
        self
    }

    pub fn vendor<'b>(&'b mut self, vendor: &'a str) -> &'b mut Self {
        self.vendor = Some(vendor);
        self
    }

    pub fn product<'b>(&'b mut self, product: &'a str) -> &'b mut Self {
        self.product = Some(product);
        self
    }

    pub fn version<'b>(&'b mut self, version: &'a str) -> &'b mut Self {
        self.version = Some(version);
        self
    }

    pub fn update<'b>(&'b mut self, update: &'a str) -> &'b mut Self {
        self.update = Some(update);
        self
    }

    pub fn edition<'b>(&'b mut self, edition: &'a str) -> &'b mut Self {
        self.edition = Some(edition);
        self
    }

    pub fn language<'b>(&'b mut self, language: &'a str) -> &'b mut Self {
        self.language = Some(language);
        self
    }
}

impl<'a> CpeBuilder<'a, Wfn<'a>> {
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

    pub fn sw_edition<'b>(&'b mut self, sw_edition: &'a str) -> &'b mut Self {
        self.sw_edition = Some(sw_edition);
        self
    }

    pub fn target_sw<'b>(&'b mut self, target_sw: &'a str) -> &'b mut Self {
        self.target_sw = Some(target_sw);
        self
    }

    pub fn target_hw<'b>(&'b mut self, target_hw: &'a str) -> &'b mut Self {
        self.target_hw = Some(target_hw);
        self
    }

    pub fn other<'b>(&'b mut self, other: &'a str) -> &'b mut Self {
        self.other = Some(other);
        self
    }
}

impl<'a> CpeBuilder<'a, Uri<'a>> {
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
        add_field!(language, set_product);

        Ok(uri)
    }
}
