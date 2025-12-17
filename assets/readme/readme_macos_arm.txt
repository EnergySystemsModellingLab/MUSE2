This folder contains the MUSE2 executable for macOS (Apple Silicon), called `muse2`.

When you first attempt to run muse2 from the console, you will probably see an error message about possible malware (this is because it is unsigned code). To fix this, you first need to run:

    xattr -d com.apple.quarantine ./muse2

After this step, you should be able to run muse2 as normal.

For more information on how to use MUSE2, you can consult the program help:

    ./muse2 help

Documentation is also available on the web:
    https://energysystemsmodellinglab.github.io/MUSE2/

Please report bugs on our GitHub issue tracker:
    https://github.com/EnergySystemsModellingLab/MUSE2/issues
