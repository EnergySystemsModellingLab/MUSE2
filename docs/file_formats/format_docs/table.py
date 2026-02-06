from table2md import MarkdownTable
from yaml import dump


def to_yaml_str(value) -> str:
    return dump(value).removesuffix("\n...\n")


def fields2table(fields: list[dict[str, str]]) -> str:
    data = []
    for f in fields:
        notes = f.get("notes", "")

        default = f.get("default", None)
        if default is not None:
            notes = f"Optional. Defaults to `{to_yaml_str(default)}`.\n\n{notes}"

        # MarkdownTable can't handle newlines, so replace with HTML equivalent
        notes = notes.replace("\n\n", "<br /><br />").replace("\n", " ")
        description = f["description"].replace("\n\n", "<br /><br />").replace("\n", " ")

        row = {
            "Field": f"`{f['name']}`",
            "Description": description,
            "Notes": notes,
        }
        data.append(row)
    return str(MarkdownTable.from_dicts(data))
