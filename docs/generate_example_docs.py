#!/usr/bin/env python3
#
# A script to generate documentation for the different examples from README.txt files.

from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from jinja2 import Environment, FileSystemLoader

DOCS_DIR = Path(__file__).parent
EXAMPLES_DIR = DOCS_DIR.parent / "examples"
OUT_PATH = DOCS_DIR / "examples.md"


@dataclass
class Example:
    name: str
    readme: str


def get_examples() -> Iterator[Example]:
    paths = sorted(example for example in EXAMPLES_DIR.iterdir() if example.is_dir())
    for path in paths:
        readme = (path / "README.txt").read_text(encoding="utf8")
        yield Example(path.name, readme)


def main():
    env = Environment(loader=FileSystemLoader(DOCS_DIR / "templates"))
    template = env.get_template("examples.md.jinja")
    out = template.render(examples=get_examples())

    print(f"Writing {OUT_PATH}")
    OUT_PATH.write_text(out, encoding="utf8")


if __name__ == "__main__":
    main()
