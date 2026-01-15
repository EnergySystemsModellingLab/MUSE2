#!/bin/sh
set -ef

mydir=$(dirname "$0")
cd "$mydir"

echo Building MUSE2
cargo -q build

if [ "$1" = --patch ]; then
    shift 1
    patch_examples=$@
elif [ $# -gt 0 ]; then
    examples=$@
else
    examples=$(cargo -q run example list)
    patch_examples=$(cargo -q run example list --patch)
fi

run_example() {
    example=$1
    debug=$2
    shift 2  # allow for passing extra args

    echo Generating data for example: $example

    env MUSE2_LOG_LEVEL=error \
        cargo -q run example run -o "data/$example" "$example" \
            --overwrite --debug-model=$debug $@
}

for example in $examples; do
    # Skip the circularity example
    if [ "$example" = circularity ]; then
        continue
    fi

    # We only need debug files for the simple model
    debug=false
    if [ "$example" = simple ]; then
        debug=true
    fi

    run_example "$example" $debug
done

for example in $patch_examples; do
    run_example "$example" false --patch
done
