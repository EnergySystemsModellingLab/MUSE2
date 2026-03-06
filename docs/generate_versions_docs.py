#!/usr/bin/env python3
# /// script
# dependencies = [
#     "jinja2",
# ]
# ///
#
# A script to generate the versions.md file, listing links to old versions of documentation.

import sys
from pathlib import Path

from jinja2 import Environment, FileSystemLoader

DOCS_DIR = Path(__file__).parent.absolute()

sys.path.append(str(DOCS_DIR / "release"))
from release import get_releases  # noqa: E402


def generate_versions_md() -> None:
    """Write the versions.md file."""
    print("Generating versions.md")
    env = Environment(loader=FileSystemLoader(DOCS_DIR / "templates"))
    template = env.get_template("versions.md.jinja")
    out = template.render(releases=get_releases())

    with (DOCS_DIR / "versions.md").open("w") as f:
        f.write(out)


if __name__ == "__main__":
    generate_versions_md()
