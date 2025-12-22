//! Check the CITATION.cff file
use anyhow::{Context, Result};
use std::fs;
use yaml_rust2::{Yaml, YamlLoader};

fn get_version_from_citation_cff() -> Result<String> {
    let citation = fs::read_to_string("CITATION.cff")?;
    let yaml = YamlLoader::load_from_str(&citation)?;
    let yaml = yaml
        .first()
        .context("Empty YAML file")?
        .as_hash()
        .context("Not YAML object")?;
    let version = yaml
        .get(&Yaml::from_str("version"))
        .context("version key not found")?
        .as_str()
        .context("version should be string")?;

    Ok(version.to_string())
}

#[test]
fn citation_cff_version() {
    assert_eq!(
        env!("CARGO_PKG_VERSION"),
        get_version_from_citation_cff().unwrap(),
        "Software version in Cargo.toml and CITATION.cff must match. If you are making a new \
        release, please also update the CITATION.cff file."
    );
}
