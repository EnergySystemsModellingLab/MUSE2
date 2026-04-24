# Applying custom HiGHS options

As part of development, you may wish to directly set custom options for the HiGHS solver. Note that
while some of these options will not affect results of simulations (e.g. to enable console logging
for HiGHS), as we cannot guarantee this for all options, in order to use this feature, you have to
set `please_give_me_broken_results = true` in your [`model.toml` file][model.toml].

You can change any of the options exposed by the HiGHS solver; for more information, see [the HiGHS
documentation][highs-opts].

You can set options to be applied to all optimisations, just dispatch or just appraisal.

Here is an example:

```toml
milestone_years = [2020, 2030, 2040]

# These options are applied to all optimisations
[highs.global_options]
# These two options are required to be enabled to log to console
log_to_console = true
output_flag = true

# These ones are just applied to dispatch
[highs.dispatch_options]
# Increase to higher than default
primal_feasibility_tolerance = 10e-6

# These ones are just applied to appraisal
[highs.appraisal_options]
optimality_tolerance = 10e-6
```

[model.toml]: https://energysystemsmodellinglab.github.io/MUSE2/file_formats/input_files.html#model-parameters-modeltoml
[highs-opts]: https://ergo-code.github.io/HiGHS/stable/options/definitions/
