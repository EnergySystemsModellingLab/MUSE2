#!/usr/bin/env python3
#
# A script to move the development docs into a /dev directory

import re
import shutil
from pathlib import Path
from tempfile import TemporaryDirectory

from release import get_releases

REPO_ROOT = Path(__file__).parent.parent.absolute()


def write_root_404(bookdir: Path) -> None:
    """Generate book/404.html from book/dev/404.html with corrected asset paths."""
    content = (bookdir / "dev" / "404.html").read_text(encoding="utf-8")

    # Replace base href with the GitHub Pages project root
    content = re.sub(r'<base\s+href="[^"]*"\s*/?>', '<base href="/MUSE2/">', content)

    # Prefix all relative href/src values with dev/
    content = re.sub(
        r'((?:href|src)=")((?!(?:https?:|//|#|/)))',
        r"\1dev/\2",
        content,
    )

    # Fix JS path variables
    content = content.replace(
        'const path_to_root = "";', 'const path_to_root = "dev/";'
    )
    content = content.replace('var pathToRoot = "";', 'var pathToRoot = "dev/";')
    content = content.replace(
        'window.path_to_searchindex_js = "',
        'window.path_to_searchindex_js = "dev/',
    )

    (bookdir / "404.html").write_text(content, encoding="utf-8")


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

    # Create a fix for the 404 routing
    write_root_404(bookdir)


if __name__ == "__main__":
    move_to_dev()
