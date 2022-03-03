# NERACOOS_CPR_DATA
Project to integrate Continuous Plankton Recorder data into NERACOOS


### About:

This repository contains the necessary code and documentation to support hosting the [Gulf of Maine Continuous Plankton Recorder Survey]() Data on [ERDDAP]().

Data in its original form can be found in the [data_raw/](www.github.com/gulfofmaine/continuous_plankton_recorder/data_raw) sub-directory.

Any modifications to the original files have been documented in the R scripts held in the [R/](www.github.com/gulfofmaine/R) sub-directory. 

All processing steps have been implemented using the [{targets}]() R-package, and can be recreated in full by running:

```
library(targets)
tar_make()
```

In an active R session. (Assuming all R-packages are installed)



