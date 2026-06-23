#!/usr/bin/env python3
#
# A script to move the development docs into a release/dev directory

import shutil
from pathlib import Path
from tempfile import TemporaryDirectory

from release import get_releases

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

    # Redirect to stable (most recent) version of docs
    with (bookdir / "index.html").open("w", encoding="utf-8") as f:
        f.write(f"""<head>
    <meta
        http-equiv="Refresh"
        content="0; URL={DOCS_SITE_ROOT}/release/{get_releases()[0]}/index.html"
    />
</head>""")


if __name__ == "__main__":
    move_to_dev()
