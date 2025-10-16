"""
Generate a simple diff between two CSV-like files.
Output format:
- lines to delete: prefixed with '-'
- lines to add: prefixed with '+'
Header line is ignored (assumes first line is the same)
"""

import sys
from pathlib import Path
import shutil


def create_diffs(fileA_path, fileB_path, diff_file):
    with open(fileA_path) as f:
        linesA = [line.strip() for line in f if line.strip()]
    with open(fileB_path) as f:
        linesB = [line.strip() for line in f if line.strip()]

    # Assume first line is header
    headerA, dataA = linesA[0], set(linesA[1:])
    headerB, dataB = linesB[0], set(linesB[1:])

    # Make sure header is the same
    if headerA != headerB:
        print(f"Header mismatch: {headerA} != {headerB}")
        sys.exit(1)

    # Lines to delete from A to get B
    to_delete = dataA - dataB

    # Lines to add to A to get B
    to_add = dataB - dataA

    # Write diff to file only if there are differences
    if to_delete or to_add:
        with open(diff_file, "w") as f:
            f.write(headerA + "\n")
            for line in to_delete:
                f.write(f"- {line}\n")
            for line in to_add:
                f.write(f"+ {line}\n")


def create_diffs_for_models(modelA_folder, modelB_folder, diffs_folder):
    modelA_folder = Path(modelA_folder)
    modelB_folder = Path(modelB_folder)
    diffs_folder = Path(diffs_folder)

    # Clear and create the diffs folder
    if diffs_folder.exists():
        shutil.rmtree(diffs_folder)
    diffs_folder.mkdir(parents=True, exist_ok=True)

    for modelA_file in modelA_folder.glob("*.csv"):
        modelB_file = modelB_folder / modelA_file.name
        diff_file = diffs_folder / modelA_file.name.replace(".csv", ".diff")
        create_diffs(modelA_file, modelB_file, diff_file)

    # Create settings.toml
    settings_file = diffs_folder / "model.toml"
    with open(settings_file, "w") as f:
        # Use forward slashes for cross-platform compatibility
        standardized_path = modelA_folder.as_posix()
        f.write(f'base_model = "{standardized_path}"\n')


create_diffs_for_models(
    "examples/muse1_default", "examples/two_regions", "examples_diffs/two_regions"
)
