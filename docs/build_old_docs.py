#!/usr/bin/env python3
#
# A script to generate documentation for previous releases of MUSE2.

import shutil
import subprocess as sp
import sys
from pathlib import Path
from tempfile import TemporaryDirectory

REPO_ROOT = Path(__file__).parent.parent.absolute()
DOCS_DIR = REPO_ROOT / "docs"

sys.path.append(str(DOCS_DIR / "release"))
from release import get_releases  # noqa: E402


def clone_repo_to(dest: Path):
    """Clone this repo somewhere else."""
    print("Making a copy of repo")
    sp.run(("git", "clone", REPO_ROOT, dest), check=True, capture_output=True)


def build_docs_for_release(release: str, repo_path: Path, outdir: Path) -> None:
    """Build documentation for a given release."""
    print(f"Building docs for {release}")

    # Check out release
    sp.run(
        ("git", "-C", str(repo_path), "checkout", release),
        check=True,
        capture_output=True,
    )

    # Apply patch, if there is one
    patch_path = DOCS_DIR / "release" / "patches" / f"{release}.patch"
    if patch_path.exists():
        sp.run(("git", "-C", str(repo_path), "am", str(patch_path)), check=True)

    # Build docs
    sp.run(("just", f"{repo_path!s}/build-docs"), check=True)

    # Move to output directory
    release_outdir = outdir / release
    print(f"Copying to {release_outdir}")
    shutil.move((repo_path / "book"), release_outdir)


def build_old_docs() -> None:
    """Build documentation for previous releases."""
    outdir = REPO_ROOT / "book" / "release"
    outdir.mkdir(parents=True, exist_ok=True)

    # Clone this repo to a temporary directory
    tmpdir = TemporaryDirectory()
    repo_path = Path(tmpdir.name)
    clone_repo_to(repo_path)

    # Generate documentation for each previous release
    for release in get_releases():
        build_docs_for_release(release, repo_path, outdir)


if __name__ == "__main__":
    build_old_docs()
