"""Convert Criterion benchmark results to the JSON format expected by
github-action-benchmark (customSmallerIsBetter).

Criterion writes one estimates.json file per benchmark under:
    target/criterion/<group>/<bench_id>/new/estimates.json

where any '/' in a group name is sanitised to '_' by Criterion when creating
directory names.  The benchmark name used for tracking is derived by stripping
the leading target/criterion/ prefix and the trailing /new/estimates.json
suffix, then joining the remaining path components with '/'.

The output is written to benchmark-results.json in the repository root, in the
format:
    [{"name": "<name>", "value": <median_ns>, "unit": "ns"}, ...]

All paths are resolved relative to the repository root (the parent directory of
the directory containing this script), so the script can be invoked from any
working directory.
"""

import json
from pathlib import Path

repo_root = Path(__file__).parent.parent
criterion_dir = repo_root / "target" / "criterion"
output_file = repo_root / "benchmark-results.json"

results = []
for estimates_file in sorted(criterion_dir.rglob("new/estimates.json")):
    parts = estimates_file.relative_to(criterion_dir).parts[:-2]
    name = "/".join(parts)
    data = json.loads(estimates_file.read_text())
    value = data["median"]["point_estimate"]
    results.append({"name": name, "value": value, "unit": "ns"})

output_file.write_text(json.dumps(results, indent=2))
print(f"Converted {len(results)} Criterion benchmark results to {output_file}")
for r in results:
    print(f"  {r['name']}: {r['value']:.0f} ns")
