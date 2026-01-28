#!/bin/sh
# add_column.sh
# Adds a new column to a specified CSV file across all example subdirectories
# Usage: add_column.sh <csv_filename> <column_name> <default_value>

set -e

# Validate command line arguments
if [ $# -ne 3 ]; then
    echo "Usage: $0 <csv_filename> <column_name> <default_value>" >&2
    echo "Example: $0 commodities.csv units NOUNIT" >&2
    exit 1
fi

csv_filename=$1
column_name=$2
default_value=$3

# Determine the examples directory relative to the tests directory
script_dir=$(dirname "$0")
examples_dir="$script_dir/../examples"

# Verify the examples directory exists
if [ ! -d "$examples_dir" ]; then
    echo "Error: Examples directory not found at $examples_dir" >&2
    exit 1
fi

# Process each subdirectory in the examples folder
for subdir in "$examples_dir"/*; do
    # Skip if not a directory
    [ -d "$subdir" ] || continue

    csv_path="$subdir/$csv_filename"

    # Skip if the CSV file doesn't exist in this subdirectory
    if [ ! -f "$csv_path" ]; then
        continue
    fi

    echo "Processing: $csv_path"

    # Create a temporary file for the modified CSV
    temp_file=$(mktemp)

    # Read and process the CSV file
    first_line=true
    while IFS= read -r line || [ -n "$line" ]; do
        if $first_line; then
            # Append new column name to header
            echo "$line,$column_name" >> "$temp_file"
            first_line=false
        else
            # Append default value to each data row
            echo "$line,$default_value" >> "$temp_file"
        fi
    done < "$csv_path"

    # Replace original file with modified version
    mv "$temp_file" "$csv_path"
done

echo "Column addition complete"
