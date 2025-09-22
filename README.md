<!-- markdownlint-disable MD041 -->
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-5-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->
[![Build and test](https://github.com/EnergySystemsModellingLab/MUSE_2.0/actions/workflows/cargo-test.yml/badge.svg)](https://github.com/EnergySystemsModellingLab/MUSE_2.0/actions/workflows/cargo-test.yml)
[![codecov](https://codecov.io/github/EnergySystemsModellingLab/MUSE_2.0/graph/badge.svg?token=nV8gp1NCh8)](https://codecov.io/github/EnergySystemsModellingLab/MUSE_2.0)
[![GitHub](https://img.shields.io/github/license/EnergySystemsModellingLab/MUSE_2.0)](https://raw.githubusercontent.com/EnergySystemsModellingLab/MUSE_2.0/main/LICENSE)

# MUSE 2.0

MUSE 2.0 (**M**od**U**lar energy systems **S**imulation **E**nvironment) is a tool for running
simulations of energy systems, written in Rust. Its purpose is to provide users with a framework to
simulate pathways of energy system transition, usually in the context of climate change mitigation.

It is the successor to [MUSE], which is written in Python. It was developed following re-design of
MUSE to address a range of legacy issues that are challenging to address via upgrades to the
existing MUSE framework, and to implement the framework in the high-performance Rust language.

:construction: **Please note that this code is under heavy development and is not yet suitable for
end users. Watch this space!** :construction:

## Model Overview

MUSE is an [Integrated Assessment Modelling] framework that is designed to enable users to create
and apply an agent-based model to simulate a market equilibrium on a set of user-defined
commodities, over a user-defined time period, for a user-specified region or set of regions. MUSE
was developed to simulate approaches to climate change mitigation over a long time horizon (e.g.
5-year steps to 2050 or 2100), but the framework is generalised and can therefore simulate any
market equilibrium.

It is a recursive dynamic modelling framework in the sense that it iterates on a single time period
to find a market equilibrium, and then moves to the next time period. Agents in MUSE have limited
foresight, reacting only to information available in the current time period. This is distinct from
intertemporal optimisation modelling frameworks (such as [TIMES] and [MESSAGEix]) which have perfect
foresight over the whole modelled time horizon.

[MUSE]: https://github.com/EnergySystemsModellingLab/MUSE_OS
[Integrated Assessment Modelling]: https://unfccc.int/topics/mitigation/workstreams/response-measures/modelling-tools-to-assess-the-impact-of-the-implementation-of-response-measures/integrated-assessment-models-iams-and-energy-environment-economy-e3-models
[TIMES]: https://iea-etsap.org/index.php/etsap-tools/model-generators/times
[MESSAGEix]: https://docs.messageix.org/en/latest

## Getting started

To start using MUSE 2.0, please refer to [the documentation]. If you wish to develop MUSE 2.0 or
build it from source, please see [the developer guide].

[the documentation]: https://energysystemsmodellinglab.github.io/MUSE_2.0/introduction.html
[the developer guide]: https://energysystemsmodellinglab.github.io/MUSE_2.0/developer_guide.html

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://www.imperial.ac.uk/admin-services/ict/self-service/research-support/rcs/service-offering/research-software-engineering/"><img src="https://avatars.githubusercontent.com/u/23149834?v=4?s=100" width="100px;" alt="Alex Dewar"/><br /><sub><b>Alex Dewar</b></sub></a><br /><a href="https://github.com/EnergySystemsModellingLab/MUSE_2.0/commits?author=alexdewar" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/tsmbland"><img src="https://avatars.githubusercontent.com/u/23723407?v=4?s=100" width="100px;" alt="Tom Bland"/><br /><sub><b>Tom Bland</b></sub></a><br /><a href="https://github.com/EnergySystemsModellingLab/MUSE_2.0/commits?author=tsmbland" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Sahil590"><img src="https://avatars.githubusercontent.com/u/56438860?v=4?s=100" width="100px;" alt="Sahil Raja"/><br /><sub><b>Sahil Raja</b></sub></a><br /><a href="https://github.com/EnergySystemsModellingLab/MUSE_2.0/commits?author=Sahil590" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Ashmit8583"><img src="https://avatars.githubusercontent.com/u/137117727?v=4?s=100" width="100px;" alt="Ashmit8583"/><br /><sub><b>Ashmit8583</b></sub></a><br /><a href="https://github.com/EnergySystemsModellingLab/MUSE_2.0/commits?author=Ashmit8583" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://www.imperial.ac.uk/admin-services/ict/self-service/research-support/rcs/service-offering/research-software-engineering/"><img src="https://avatars.githubusercontent.com/u/6095790?v=4?s=100" width="100px;" alt="Diego Alonso Álvarez"/><br /><sub><b>Diego Alonso Álvarez</b></sub></a><br /><a href="https://github.com/EnergySystemsModellingLab/MUSE_2.0/commits?author=dalonsoa" title="Code">💻</a></td>
    </tr>
  </tbody>
  <tfoot>
    <tr>
      <td align="center" size="13px" colspan="7">
        <img src="https://raw.githubusercontent.com/all-contributors/all-contributors-cli/1b8533af435da9854653492b1327a23a4dbd0a10/assets/logo-small.svg">
          <a href="https://all-contributors.js.org/docs/en/bot/usage">Add your contributions</a>
        </img>
      </td>
    </tr>
  </tfoot>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors)
specification. Contributions of any kind welcome!

## Copyright

Copyright © 2025 Imperial College London
