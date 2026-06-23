#!/usr/bin/env python3
#
# A script to move the development docs into a release/dev directory

import shutil
from pathlib import Path
from tempfile import TemporaryDirectory

DOCS_SITE_ROOT = "https://energysystemsmodellinglab.github.io/MUSE2"
REPO_ROOT = Path(__file__).parent.parent.absolute()


def move_to_dev() -> None:
    """Build documentation for previous releases."""
    bookdir = REPO_ROOT / "book"
    outdir = REPO_ROOT / "book" / "release" / "dev"

    with TemporaryDirectory() as tmpdir:
        # Move book to temporary directory
        shutil.move(bookdir, tmpdir)
        shutil.move(Path(tmpdir) / "book", outdir)


if __name__ == "__main__":
    move_to_dev()
