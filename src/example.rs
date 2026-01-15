//! Code for working with example models
use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use include_dir::{Dir, DirEntry, include_dir};

pub mod patches;

/// The directory containing the example models.
const EXAMPLES_DIR: Dir = include_dir!("examples");

/// Get the names of all examples
pub fn get_example_names() -> impl Iterator<Item = &'static str> {
    EXAMPLES_DIR.dirs().map(|dir| {
        dir.path()
            .as_os_str()
            .to_str()
            .expect("Invalid unicode in path")
    })
}

/// A bundled example model
pub struct Example(Dir<'static>);

impl Example {
    /// Get the example with the specified name
    pub fn from_name(name: &str) -> Result<Self> {
        let dir = EXAMPLES_DIR
            .get_dir(name)
            .with_context(|| format!("Example '{name}' not found"))?;

        Ok(Self(dir.clone()))
    }

    /// Get the contents of the readme file for this example
    pub fn get_readme(&self) -> Result<&'static str> {
        self.0
            .get_file(self.0.path().join("README.txt"))
            .context("Missing file")?
            .contents_utf8()
            .context("File not UTF-8 encoded")
    }

    /// Extract this example to a specified destination
    pub fn extract(&self, new_path: &Path) -> Result<()> {
        // Copy the contents of the subdirectory to the destination
        fs::create_dir(new_path)?;
        for entry in self.0.entries() {
            match entry {
                DirEntry::Dir(_) => panic!("Subdirectories in examples not supported"),
                DirEntry::File(f) => {
                    let file_name = f.path().file_name().unwrap();
                    let file_path = new_path.join(file_name);
                    fs::write(&file_path, f.contents())?;
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_examples_have_readme() {
        for example in get_example_names() {
            let readme = Example::from_name(example)
                .unwrap()
                .get_readme()
                .with_context(|| format!("Could not load readme for {example}"))
                .unwrap();

            assert!(!readme.trim().is_empty());
        }
    }
}
