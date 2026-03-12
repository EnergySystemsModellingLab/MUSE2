#!/usr/bin/env python3
# /// script
# dependencies = [
#     "jinja2",
# ]
# ///
#
# A script to generate the versions.md file, listing links to old versions of documentation.

from pathlib import Path

from jinja2 import Environment, FileSystemLoader
from release import get_releases

DOCS_DIR = Path(__file__).parent.absolute()


def generate_versions_md() -> None:
    """Write the versions.md file."""
    path = DOCS_DIR / "versions.md"
    print(f"Writing {path}")
    env = Environment(loader=FileSystemLoader(DOCS_DIR / "templates"))
    template = env.get_template("versions.md.jinja")
    out = template.render(releases=get_releases())

    with path.open("w", encoding="utf-8") as f:
        f.write(out)


if __name__ == "__main__":
    generate_versions_md()
