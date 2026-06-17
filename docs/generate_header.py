#!/usr/bin/env python3
#
# A script to generate theme/header.hbs from a Jinja2 template.

from pathlib import Path

from jinja2 import Environment, FileSystemLoader
from release import get_releases

DOCS_DIR = Path(__file__).parent.absolute()
REPO_ROOT = DOCS_DIR.parent


def generate_header_hbs() -> None:
    """Write the theme/header.hbs file."""
    path = REPO_ROOT / "theme" / "header.hbs"
    print(f"Writing {path}")
    env = Environment(loader=FileSystemLoader(DOCS_DIR / "templates"))
    template = env.get_template("header.hbs.jinja")
    out = template.render(releases=get_releases())

    with path.open("w", encoding="utf-8") as f:
        f.write(out)


if __name__ == "__main__":
    generate_header_hbs()
