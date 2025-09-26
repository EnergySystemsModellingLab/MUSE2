# User Guide

## Running MUSE 2.0

Once you have installed MUSE 2.0, you should be able to run it via the `muse2` command-line program.
For details of the command-line interface, [see here](./command_line_help.md).

## Modifying the program settings

You can configure the behaviour of MUSE 2.0 with the `settings.toml` file. To edit this file, run:

```sh
muse2 settings edit
```

There are also some more commands for working with the settings file; for details, run: `muse2
settings help`.

For information about the available settings, see [the documentation for the `settings.toml`
file][settings.toml-docs].

[settings.toml-docs]:
https://energysystemsmodellinglab.github.io/MUSE_2.0/file_formats/program_settings.html

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
