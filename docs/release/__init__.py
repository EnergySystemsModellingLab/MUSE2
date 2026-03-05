"""Common functionality for working with different versions."""

import re
import subprocess as sp


def is_release_tag(tag: str) -> bool:
    """Whether the git tag indicates a version.

    We don't include pre-releases.
    """
    return re.match(r"^v[0-9]+\.[0-9]+\.[0-9]+$", tag) is not None


def get_releases() -> list[str]:
    """Get all release tags for this repo."""
    ret = sp.run(("git", "tag"), capture_output=True, check=True, encoding="utf-8")
    return [tag for tag in ret.stdout.splitlines() if is_release_tag(tag)]
