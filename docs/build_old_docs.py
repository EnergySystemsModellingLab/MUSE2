#!/usr/bin/env python3
#
# A script to generate documentation for previous releases of MUSE2.

import os
import shutil
import subprocess as sp
from pathlib import Path
from tempfile import TemporaryDirectory

from release import get_releases

DOCS_SITE_ROOT = "https://energysystemsmodellinglab.github.io/MUSE2"
REPO_ROOT = Path(__file__).parent.parent.absolute()
DOCS_DIR = REPO_ROOT / "docs"


def clone_repo_to(dest: Path):
    """Clone this repo somewhere else."""
    print(f"Making a copy of repo in {dest}")
    sp.run(("git", "clone", REPO_ROOT, dest), check=True, capture_output=True)

    # Add a symlink to cargo cache dir
    try:
        os.symlink(REPO_ROOT / "target", dest / "target")
    except (NotImplementedError, OSError):
        # Only newer versions of Windows support symlinks and these require the user to have
        # additional privileges (or to be in developer mode)
        print(
            "WARN: Could not create symlink to cache directory; cache will not be stored"
        )


def apply_patches_for_release(release: str, repo_path: Path) -> None:
    """Apply patches (if any) for the given release."""
    patches_dir = DOCS_DIR / "release" / "patches" / release
    for patch_path in sorted(patches_dir.glob("*.patch")):
        sp.run(("git", "-C", str(repo_path), "am", str(patch_path)), check=True)


def build_docs_for_release(release: str, repo_path: Path, outdir: Path) -> None:
    """Build documentation for a given release."""
    print(f"Building docs for {release}")

    # Check out release
    sp.run(
        ("git", "-C", str(repo_path), "checkout", release),
        check=True,
        capture_output=True,
    )

    # Apply patches, if any
    apply_patches_for_release(release, repo_path)

    # Build docs
    sp.run(("just", f"{repo_path!s}/build-docs"), check=True)

    # Patch versions.html to redirect to main versions page
    with (repo_path / "book" / "versions.html").open("w", encoding="utf-8") as f:
        f.write(f"""<head>
    <meta http-equiv="Refresh" content="0; URL={DOCS_SITE_ROOT}/versions.html" />
</head>""")

    # Move to output directory
    release_outdir = outdir / release
    print(f"Copying to {release_outdir}")
    shutil.move((repo_path / "book"), release_outdir)


def build_old_docs() -> None:
    """Build documentation for previous releases."""
    outdir = REPO_ROOT / "book" / "release"
    outdir.mkdir(parents=True, exist_ok=True)

    # Clone this repo to a temporary directory
    with TemporaryDirectory() as tmpdir:
        repo_path = Path(tmpdir)
        clone_repo_to(repo_path)

        # Generate documentation for each previous release
        for release in get_releases():
            build_docs_for_release(release, repo_path, outdir)


if __name__ == "__main__":
    build_old_docs()
