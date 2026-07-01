#!/usr/bin/env python3
#
# A script to move the development docs into a /dev directory

import shutil
from pathlib import Path
from tempfile import TemporaryDirectory

from release import get_releases

REPO_ROOT = Path(__file__).parent.parent.absolute()


def move_to_dev() -> None:
    """Move the built documentation output (`book/`) into `book/dev/`."""
    bookdir = REPO_ROOT / "book"
    outdir = REPO_ROOT / "book" / "dev"

    with TemporaryDirectory() as tmpdir:
        # Move book to temporary directory
        shutil.move(bookdir, tmpdir)
        shutil.move(Path(tmpdir) / "book", outdir)

    # Redirect to stable (most recent) version of docs
    with (bookdir / "index.html").open("w", encoding="utf-8") as f:
        f.write(f"""<head>
    <meta
        http-equiv="Refresh"
        content="0; URL=./{get_releases()[0]}/index.html"
    />
</head>""")


if __name__ == "__main__":
    move_to_dev()
