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
RELEASE_NOTES_START_ANCHOR = "ANCHOR: release_notes"
RELEASE_NOTES_END_ANCHOR = "ANCHOR_END: release_notes"


def clone_repo_to(dest: Path):
    """Make a bare clone of this repo somewhere else."""
    print(f"Making a copy of repo in {dest}")
    sp.run(("git", "clone", "--bare", REPO_ROOT, dest), check=True, capture_output=True)


def add_worktree_for_release(repo_path: Path, release: str) -> Path:
    """Add a worktree for the current release.

    Also tries to symlink to the cargo cache dir to speed up build times, if possible.
    """
    # Add new worktree
    release_path = repo_path.parent / release
    sp.run(
        (
            "git",
            "-C",
            str(repo_path),
            "worktree",
            "add",
            release_path,
            release,
        ),
        check=True,
        capture_output=True,
    )

    # Add a symlink to cargo cache dir
    try:
        os.symlink(REPO_ROOT / "target", release_path / "target")
    except (NotImplementedError, OSError):
        # Only newer versions of Windows support symlinks and these require the user to have
        # additional privileges (or to be in developer mode)
        print(
            "WARN: Could not create symlink to cache directory; cache will not be stored"
        )

    return release_path


def apply_patches_for_release(release: str, repo_path: Path) -> None:
    """Apply patches (if any) for the given release."""
    patches_dir = DOCS_DIR / "release" / "patches" / release
    for patch_path in sorted(patches_dir.glob("*.patch")):
        sp.run(("git", "-C", str(repo_path), "am", str(patch_path)), check=True)


def patch_release_notes(release_path: Path, release_notes_toc: str) -> None:
    """Use the latest version of release notes everywhere.

    This means we can have release notes for newer versions linked to from old versions of the
    documentation.
    """
    # Patch in the contents of release_notes_toc between the anchors
    new_summary = ""
    found_start_anchor = False
    summary_path = release_path / "docs" / "SUMMARY.md"
    with summary_path.open(encoding="utf-8") as f:
        lines = iter(f.readlines())

        for line in lines:
            if RELEASE_NOTES_START_ANCHOR in line:
                assert not found_start_anchor
                found_start_anchor = True
                new_summary += release_notes_toc

                # Skip ahead until we find end anchor
                try:
                    next(line for line in lines if RELEASE_NOTES_END_ANCHOR in line)
                except StopIteration:
                    raise RuntimeError(
                        "End anchor for release notes not found in SUMMARY.md"
                    )
            else:
                new_summary += line
        if not found_start_anchor:
            raise RuntimeError("Start anchor for release notes not found in SUMMARY.md")
    with summary_path.open("w", encoding="utf-8") as f:
        f.write(new_summary)

    # Replace release_notes dir with symlink/copy of version in main repo
    release_notes_path = release_path / "docs" / "release_notes"
    if release_notes_path.exists():
        shutil.rmtree(release_notes_path)
    try:
        os.symlink(
            DOCS_DIR / "release_notes", release_notes_path, target_is_directory=True
        )
    except (NotImplementedError, OSError):
        # Only newer versions of Windows support symlinks and these require the user to have
        # additional privileges (or to be in developer mode)
        print("WARN: Could not create symlink to release notes. Falling back on copy.")
        shutil.copytree(DOCS_DIR / "release_notes", release_notes_path)


def build_docs_for_release(
    release: str, release_notes_toc: str, repo_path: Path, outdir: Path
) -> None:
    """Build documentation for a given release."""
    print(f"Building docs for {release}")
    release_path = add_worktree_for_release(repo_path, release)

    # Apply patches, if any
    apply_patches_for_release(release, release_path)

    # Use release notes from main version of docs
    patch_release_notes(release_path, release_notes_toc)

    # Build docs
    sp.run(("just", f"{release_path!s}/build-docs"), check=True)

    # Patch versions.html to redirect to main versions page
    with (release_path / "book" / "versions.html").open("w", encoding="utf-8") as f:
        f.write(f"""<head>
    <meta http-equiv="Refresh" content="0; URL={DOCS_SITE_ROOT}/versions.html" />
</head>""")

    # Move to output directory
    release_outdir = outdir / release
    print(f"Copying to {release_outdir}")
    shutil.move((release_path / "book"), release_outdir)


def read_release_notes_toc() -> str:
    """Read the release notes section from SUMMARY.md."""
    with (DOCS_DIR / "SUMMARY.md").open(encoding="utf-8") as f:
        lines = iter(f.readlines())

        # Skip ahead until we find start anchor
        try:
            next(line for line in lines if RELEASE_NOTES_START_ANCHOR in line)
        except StopIteration:
            raise RuntimeError("Start anchor for release notes not found in SUMMARY.md")

        # Append lines to output until we find end anchor
        out = ""
        for line in lines:
            if RELEASE_NOTES_END_ANCHOR in line:
                return out
            out += line

        raise RuntimeError("End anchor for release notes not found in SUMMARY.md")


def build_old_docs() -> None:
    """Build documentation for previous releases."""
    release_notes_toc = read_release_notes_toc()

    outdir = REPO_ROOT / "book" / "release"
    outdir.mkdir(parents=True, exist_ok=True)

    # Clone this repo to a temporary directory
    with TemporaryDirectory() as tmpdir:
        repo_path = Path(tmpdir) / "MUSE2.git"
        clone_repo_to(repo_path)

        # Generate documentation for each previous release
        for release in get_releases():
            build_docs_for_release(release, release_notes_toc, repo_path, outdir)


if __name__ == "__main__":
    build_old_docs()
