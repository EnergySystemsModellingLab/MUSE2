# User Guide

Once you have installed MUSE2, you should be able to run it via the `muse2` command-line program.
For details of the command-line interface, [see here](./command_line_help.md) or run `muse2 help`.

## Example models

MUSE2 comes with a number of [example models], partly to demonstrate the various program features as
well as to be used as templates for new models. To see the list of available examples, you can run:

```sh
muse2 example list
```

It should print something like the following:

```sh
missing_commodity
muse1_default
simple
two_outputs
two_regions
```

To view information about a particular example, you can run, e.g.:

```sh
muse2 example info simple
```

You can run examples like so:

```sh
muse2 example run simple
```

To see the input files for an example (e.g. to use as a template for your own model), run:

```sh
muse2 example extract simple
```

[example models]: ./examples.md

## Visualising commodity graphs

To visualise the structure of your model, you can use the [the `muse2 save-graphs` command] to
create graphs of commodity/process relationships.
This command will output a graph for each region/year in the simulation, where nodes are commodities
and edges are processes.
Graphs will be saved in [DOT format], which can be visualised locally with [Graphviz], or online
with [Graphviz online].

[the `muse2 save-graphs` command]: https://energysystemsmodellinglab.github.io/MUSE2/command_line_help.html#muse2-save-graphs
[DOT format]: https://graphviz.org/doc/info/lang.html
[Graphviz]: https://graphviz.org/
[Graphviz online]: https://dreampuf.github.io/GraphvizOnline

## Modifying the program settings

You can configure the behaviour of MUSE2 with a `settings.toml` file. To edit this file, run:

```sh
muse2 settings edit
```

There are also some more commands for working with the settings file; for details, run: `muse2
settings help`.

For information about the available settings, see [the documentation for the `settings.toml`
file][settings.toml-docs].

[settings.toml-docs]:
https://energysystemsmodellinglab.github.io/MUSE2/file_formats/program_settings.html

## Setting the log level

MUSE uses the [`fern`] crate for logging. The default log level is `info`, though this can be
configured either via the `log_level` option in `settings.toml` or by setting the
`MUSE2_LOG_LEVEL` environment variable. (If both are used, the environment variable takes
precedence.)

The possible options are:

- `error`
- `warn`
- `info`
- `debug`
- `trace`
- `off`

By default, MUSE will colourise the log output if this is available (i.e. it is outputting to a
terminal rather than a file).

For more information, please consult [the `fern` documentation].

[`fern`]: https://crates.io/crates/fern
[the `fern` documentation]: https://docs.rs/fern/latest/fern/
