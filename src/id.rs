//! Code for handling IDs
use anyhow::{Context, Result};
use indexmap::{IndexMap, IndexSet};
use std::borrow::Borrow;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

/// A trait alias for ID types
pub trait IDLike: Eq + Hash + Borrow<str> + Clone + Display + From<String> {}
impl<T> IDLike for T where T: Eq + Hash + Borrow<str> + Clone + Display + From<String> {}

macro_rules! define_id_type {
    ($name:ident) => {
        #[derive(
            Clone,
            derive_more::Display,
            std::hash::Hash,
            PartialOrd,
            Ord,
            PartialEq,
            Eq,
            Debug,
            serde::Serialize,
        )]
        /// An ID type (e.g. `AgentID`, `CommodityID`, etc.)
        pub struct $name(pub std::rc::Rc<str>);

        impl std::borrow::Borrow<str> for $name {
            fn borrow(&self) -> &str {
                &self.0
            }
        }

        impl From<&str> for $name {
            fn from(s: &str) -> Self {
                $name(std::rc::Rc::from(s))
            }
        }

        impl From<String> for $name {
            fn from(s: String) -> Self {
                $name(std::rc::Rc::from(s))
            }
        }

        impl<'de> serde::Deserialize<'de> for $name {
            fn deserialize<D>(deserialiser: D) -> std::result::Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                use serde::de::Error;
                const FORBIDDEN_IDS: [&str; 2] = ["all", "annual"];

                let id: String = serde::Deserialize::deserialize(deserialiser)?;
                let id = id.trim();
                if id.is_empty() {
                    return Err(D::Error::custom("IDs cannot be empty"));
                }

                for forbidden in FORBIDDEN_IDS.iter() {
                    if id.eq_ignore_ascii_case(forbidden) {
                        return Err(D::Error::custom(format!(
                            "'{id}' is an invalid value for an ID"
                        )));
                    }
                }

                Ok(id.into())
            }
        }

        impl $name {
            /// Create a new ID from a string slice
            pub fn new(id: &str) -> Self {
                $name(std::rc::Rc::from(id))
            }
        }
    };
}
pub(crate) use define_id_type;

#[cfg(test)]
define_id_type!(GenericID);

/// Indicates that the struct has an ID field
pub trait HasID<ID: IDLike> {
    /// Get the struct's ID
    fn get_id(&self) -> &ID;
}

/// Implement the `HasID` trait for the given type, assuming it has a field called `id`
macro_rules! define_id_getter {
    ($t:ty, $id_ty:ty) => {
        impl crate::id::HasID<$id_ty> for $t {
            fn get_id(&self) -> &$id_ty {
                &self.id
            }
        }
    };
}
pub(crate) use define_id_getter;

/// A data structure containing a set of IDs
pub trait IDCollection<ID: IDLike> {
    /// Check if the ID is in the collection, returning a reference to it if found.
    ///
    /// # Arguments
    ///
    /// * `id` - The ID to check (can be string or ID type)
    ///
    /// # Returns
    ///
    /// A reference to the ID in `self`, or an error if not found.
    fn get_id<T: Borrow<str> + Display + ?Sized>(&self, id: &T) -> Result<&ID>;
}

macro_rules! define_id_methods {
    () => {
        fn get_id<T: Borrow<str> + Display + ?Sized>(&self, id: &T) -> Result<&ID> {
            let found = self
                .get(id.borrow())
                .with_context(|| format!("Unknown ID {id} found"))?;
            Ok(found)
        }
    };
}

impl<ID: IDLike> IDCollection<ID> for HashSet<ID> {
    define_id_methods!();
}

impl<ID: IDLike> IDCollection<ID> for IndexSet<ID> {
    define_id_methods!();
}

impl<ID: IDLike, V> IDCollection<ID> for IndexMap<ID, V> {
    fn get_id<T: Borrow<str> + Display + ?Sized>(&self, id: &T) -> Result<&ID> {
        let (found, _) = self
            .get_key_value(id.borrow())
            .with_context(|| format!("Unknown ID {id} found"))?;
        Ok(found)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    use serde::Deserialize;

    #[derive(Debug, Deserialize)]
    struct Record {
        id: GenericID,
    }

    fn deserialise_id(id: &str) -> Result<Record> {
        Ok(toml::from_str(&format!("id = \"{id}\""))?)
    }

    #[rstest]
    #[case("commodity1")]
    #[case("some commodity")]
    #[case("PROCESS")]
    #[case("café")] // unicode supported
    fn deserialise_id_valid(#[case] id: &str) {
        assert_eq!(deserialise_id(id).unwrap().id.to_string(), id);
    }

    #[rstest]
    #[case("")]
    #[case("all")]
    #[case("annual")]
    #[case("ALL")]
    #[case(" ALL ")]
    fn deserialise_id_invalid(#[case] id: &str) {
        deserialise_id(id).unwrap_err();
    }
}
