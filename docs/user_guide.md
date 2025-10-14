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

[example models]: ./examples.md

## Building and running your own model

> 🚧 Note that this section is under construction! 🚧
>
> In the longer term, we [plan to have a tutorial][tutorial-issue] describing how to build a model
> in more detail.

Models in MUSE2 are defined with one [TOML] file (`model.toml`) and many CSV files. For a
description of each of the files and the different fields, see [the documentation for input files].

[TOML]: https://toml.io/en/
[input-files-docs]: file_formats/input_files.md

### Creating a new model from an example

We recommend you use one of the examples as starting point for your own model, as there are many
required files.

To create a new model based on the `simple` example, run:

```sh
muse2 example extract simple new_model
```

This will create a new subdirectory called `new_model` in the current folder.

### Running this model

First, let's run this model so you can see the output for a working model. You can do this by
running:

```sh
muse2 run new_model
```

If everything works as expected, you should see output on your terminal indicating the progress of
the simulation (which should finish very quickly).

The first few lines should look something like:

```txt
[12:24:20 INFO muse2::cli] Starting MUSE2 v2.0.0
[12:24:20 INFO muse2::cli] Loaded model from new_model/
[12:24:20 INFO muse2::cli] Output folder: muse2_results/new_model
...
```

You should see that a new `muse2_results` folder has been created. This folder will contain the
output for your model in a subfolder called `new_model`. For information about how to interpret
these files, see [the documentation for output files]. We also have some [example Jupyter
notebooks].

[example Jupyter notebooks]: https://github.com/EnergySystemsModellingLab/MUSE2/tree/main/docs/notebooks

### Next steps

You will now want to configure the model for your own use case. You should start by looking at [the
documentation for the input files][input-files-docs] for details of the different data types and
parameters for MUSE2.

Unfortunately, this may not be easy, especially if you are not already familiar with [MUSE1]. In the
longer term, [we will have tutorials][tutorial-issue], so watch this space! In the meantime, if you
have a question, feel free to [open an issue].

[MUSE1]: https://github.com/EnergySystemsModellingLab/MUSE_OS
[tutorial-issue]: https://github.com/EnergySystemsModellingLab/MUSE2/issues/921
[open an issue]: https://github.com/EnergySystemsModellingLab/MUSE2/issues

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
