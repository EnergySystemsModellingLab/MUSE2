#!/usr/bin/env python3
#
# A script to generate markdown documentation from table schemas. If invoked without any arguments,
# it will generate all file format documentation. Alternatively, you can specify one or more type
# (e.g. "input") as an argument and just those files will be written.

from pathlib import Path
import sys
from typing import Iterable

from jinja2 import Environment, FileSystemLoader

FILE_FORMAT_DOCS_DIR = Path(__file__).parent
SCHEMA_DIR = FILE_FORMAT_DOCS_DIR.parent.parent / "schemas"
INPUT_SCHEMA_DIR = SCHEMA_DIR / "input"
INPUT_MODEL_FILE_NAME = "model.toml"
OUTPUT_SCHEMA_DIR = SCHEMA_DIR / "output"

INPUT_FILE_ORDER = {
    "Time slices": ["time_slices"],
    "Regions": ["regions"],
    "Agents": ["agents", "agent_*"],
    "Assets": ["assets"],
    "Commodities": ["commodities", "commodity_levies", "demand", "demand_slicing"],
    "Processes": ["processes", "process_*"],
}
OUTPUT_FILE_ORDER = {
    "Main CSV output files": [
        "assets",
        "asset_capacities",
        "commodity_flows",
        "commodity_prices",
    ]
}

sys.path.append(str(FILE_FORMAT_DOCS_DIR))
from format_docs import generate_for_csv, generate_for_toml  # noqa: E402


def generate_settings_docs(env: Environment) -> tuple[str, str]:
    toml_file_name = "settings.toml"
    out = generate_for_toml(SCHEMA_DIR, toml_file_name, env, heading_level=1)
    return ("program_settings.md", out)


def generate_input_docs(env: Environment) -> tuple[str, str]:
    csv_sections = generate_for_csv(INPUT_FILE_ORDER, INPUT_SCHEMA_DIR, env)
    toml_info = generate_for_toml(
        INPUT_SCHEMA_DIR, INPUT_MODEL_FILE_NAME, env, heading_level=2
    )

    template = env.get_template("input_files.md.jinja")
    out = template.render(
        csv_sections=csv_sections, toml_info=toml_info, script_name=Path(__file__).name
    )
    return ("input_files.md", out)


def generate_output_docs(env: Environment) -> tuple[str, str]:
    csv_sections = generate_for_csv(OUTPUT_FILE_ORDER, OUTPUT_SCHEMA_DIR, env)
    toml_file_name = "metadata.toml"
    toml_info = generate_for_toml(
        OUTPUT_SCHEMA_DIR, toml_file_name, env, heading_level=2
    )

    template = env.get_template("output_files.md.jinja")
    out = template.render(
        csv_sections=csv_sections, toml_info=toml_info, script_name=Path(__file__).name
    )
    return ("output_files.md", out)


generators = {
    "settings": generate_settings_docs,
    "input": generate_input_docs,
    "output": generate_output_docs,
}


def main(options: Iterable[str]) -> None:
    env = Environment(
        loader=FileSystemLoader(FILE_FORMAT_DOCS_DIR.parent / "templates")
    )

    for option in options:
        try:
            fun = generators[option]
        except KeyError:
            print(f'Unknown option "{option}"')
        else:
            filename, txt = fun(env)
            path = FILE_FORMAT_DOCS_DIR / filename
            print(f"Writing {path}")
            path.write_text(txt, encoding="utf-8")


if __name__ == "__main__":
    options: Iterable[str] = sys.argv[1:]
    if not options:
        options = generators.keys()

    main(options)
