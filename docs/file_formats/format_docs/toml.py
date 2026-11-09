from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml
from jinja2 import Environment

from .table import fields2table


@dataclass
class TOMLInfo:
    heading: str
    description: str
    tables: dict[str, str]


def generate_for_toml(
    schema_dir: Path, file_name: str, env: Environment, heading_level: int
) -> str:
    """Generate markdown from Jinja template using metadata in schema for TOML file."""

    file_name_stem, _, _ = file_name.rpartition(".")
    schema_path = schema_dir / f"{file_name_stem}.yaml"

    template = env.get_template("toml.md.jinja")
    toml_info = _load_toml_info(file_name, schema_path)
    return template.render(
        title=toml_info.heading,
        heading_level=heading_level * "#",
        description=toml_info.description,
        tables=toml_info.tables,
    )


def _load_toml_info(file_name: str, schema_path: Path) -> TOMLInfo:
    with schema_path.open() as f:
        data = yaml.safe_load(f)

    title = data.get("title", None)
    heading = f"{title}: `{file_name}`" if title else f"`{file_name}`"
    assert data["type"] == "object"

    properties = toml_table2list(data["properties"])
    tables = {"": fields2table(properties)}  # root table
    tables |= {
        prop["name"]: fields2table(toml_table2list(prop["properties"]))
        for prop in properties
        if prop["type"] == "object"
    }

    return TOMLInfo(heading, data.get("description", ""), tables)


def toml_table2list(props: dict[str, Any]) -> list[dict[str, Any]]:
    """Convert a TOML subtable to a list to be processed by `fields2table`.

    Nested subtables are not supported.
    """
    out = []
    for key, value in props.items():
        value["name"] = key
        out.append(value)
    return out
