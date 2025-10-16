#!/usr/bin/env python3
"""
Apply a simple diff to a CSV-like file.
- Lines starting with '-' are removed
- Lines starting with '+' are added
Header (first line) is preserved
"""

from pathlib import Path
import shutil


def apply_diff(base_file, diff_file, output_file):
    with open(base_file) as f:
        lines = [line.strip() for line in f if line.strip()]

    header, data = lines[0], set(lines[1:])

    with open(diff_file) as f:
        for line in f:
            line = line.strip()
            if line.startswith("-"):
                data.discard(line[1:].strip())
            elif line.startswith("+"):
                data.add(line[1:].strip())

    # Optional: keep data sorted, header first
    new_lines = [header] + list(data)

    # Write to file
    with open(output_file, "w") as f:
        for line in new_lines:
            f.write(f"{line}\n")


def apply_diffs_for_model(diffs_folder, output_folder):
    # Load model.toml
    diffs_folder = Path(diffs_folder)
    with open(diffs_folder / "model.toml") as f:
        model_folder = Path(f.read().strip().split("=")[1].strip().strip('"'))
    model_folder = Path(model_folder)
    output_folder = Path(output_folder)

    # Clear the output folder
    if output_folder.exists():
        shutil.rmtree(output_folder)

    # Copy the base model to the output folder as a starting point
    shutil.copytree(model_folder, output_folder)

    for diff_file in diffs_folder.glob("*.diff"):
        apply_diff(
            model_folder / diff_file.name.replace(".diff", ".csv"),
            diff_file,
            output_folder / diff_file.name.replace(".diff", ".csv"),
        )


apply_diffs_for_model("examples_diffs/two_regions", "examples/two_regions")
