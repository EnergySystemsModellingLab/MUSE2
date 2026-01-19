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
    shift 1  # allow for passing extra args

    echo Generating data for example: $example

    env MUSE2_LOG_LEVEL=error MUSE2_USE_DEFAULT_SETTINGS=1 \
        cargo -q run example run -o "data/$example" "$example" \
            --overwrite $@
}

for example in $examples; do
    # We only need debug files for the simple model
    unset extra_args
    if [ "$example" = simple ]; then
        extra_args=--debug-model
    fi

    run_example "$example" $extra_args
done

for example in $patch_examples; do
    run_example "$example" --patch
done
