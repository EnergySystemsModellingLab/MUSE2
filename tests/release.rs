//! Check the CITATION.cff file
use anyhow::{Context, Result};
use std::fs;
use std::path::Path;
use yaml_rust2::{Yaml, YamlLoader};

/// Version string for the current version of MUSE2
const MUSE2_VERSION: &str = env!("CARGO_PKG_VERSION");

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
        MUSE2_VERSION,
        get_version_from_citation_cff().unwrap(),
        "Software version in Cargo.toml and CITATION.cff must match. If you are making a new \
        release, please also update the CITATION.cff file."
    );
}

/// A crude check that release notes for the current version is referenced the given path
fn check_link_to_release_notes(path: &Path) {
    let contents = fs::read_to_string(path).unwrap();
    assert!(
        contents.contains(&format!("v{MUSE2_VERSION}.md")),
        "File {} does not contain a link to the latest version's release notes",
        path.display()
    );
}

#[test]
fn release_notes_file_exists() {
    let path = format!("docs/release_notes/v{MUSE2_VERSION}.md");
    assert!(
        Path::new(&path).exists(),
        "Release notes doc doesn't exist: {path}"
    );

    check_link_to_release_notes(Path::new("docs/SUMMARY.md"));
    check_link_to_release_notes(Path::new("docs/release_notes/README.md"));
}
