
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NERACOOS_CPR_DATA

Project to integrate Continuous Plankton Recorder data into NERACOOS &
ERDDAP

## About:

This repository contains the necessary code and documentation to support
hosting the [Gulf of Maine Continuous Plankton Recorder Survey
Data](https://www.fisheries.noaa.gov/feature-story/long-running-plankton-survey-resume-gulf-maine)
on
[ERDDAP](http://ismn.erddap.neracoos.org/erddap/info/index.html?page=1&itemsPerPage=1000).

## NOAA Gulf of Maine CPR Data

Data in its original form can be found in the
[data_raw/](www.github.com/gulfofmaine/continuous_plankton_recorder/data_raw)
sub-directory. Data in its original form was obtained from the NOAA
Northeast Fisheries Science Center through communication with Chris
Melrose.

Data obtained from NOAA/NEFSC spans the period of: 1961-2013, and was
delivered in the following excel file:

> **data_raw/NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014
> update).xlsx**

That excel file is divided into 2 sheets. One contains data on
phytoplankton and the other contains records on zooplankton. Each excel
sheet has two or more additional rows in the header explaining the
identification and development stage of certain taxa.

**NOTE: CPR data from NOAA is reported in units that do not follow CPR
survey conventions.**

| Sheet Number | Description                                   | Units             |
|--------------|-----------------------------------------------|-------------------|
| Sheet 1      | Phytoplankton taxa densities by silk transect | Abundance / 1m3   |
| Sheet 2      | Zooplankton taxa densities by silk transect   | Abundance / 100m3 |

### Data Processing Code

To make the CPR data more “ERDDAP-friendly” minor changes were applied
to reshape the structure of the original dataset, resulting in a
longer/tidyer dataset. Any modifications to the original files have been
documented in the R scripts held in the
[R/](www.github.com/gulfofmaine/R) sub-directory.

Taxonomic encodings from the original excel file headers are also
checked against MARMAP codes to ensure accuracy
<https://www.nefsc.noaa.gov/nefsc/Narragansett/taxcodesA.html>

### Reproducing Changes to CPR Data

The full processing pipeline from raw data to its ERDDAP format has been
implemented using the [{targets}](https://docs.ropensci.org/targets/)
R-package, and can be recreated in full by running the following code in
an active R session. (Assuming all R-packages are installed).

    library(targets)
    tar_make()

This will recreate the processing steps outlined in `_targets.R`:

<img src="man/figures/README-gulf of maine pipeline-1.png" width="100%" />

------------------------------------------------------------------------

## SAHFOS Gulf of Maine CPR Data

Data obtained from SAHFOS spans the period of: **2013-2017**, and was
delivered in the following files:

> **data_raw/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx**  
> **data_raw/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx**

Each of these files contain **three** sheets containing the different
counting scales of the CPR survey, the phytoplankton, the traverse, and
the “eye-count” scales. These three scales are based on the size of
organisms counted. At each scale different subsets of the silk transect
are used when counting the individually identified taxa. Counts from the
subsets are then scaled to the entire silk transect and given a number
on a categorical counting scale. These represent discrete jumps in
abundance per transect.

These three measurement increments correspond with the following
sub-sampling protocols to save time when counting very small organisms:

1.  **Phyto** - 1/8000th of transect counted  
2.  **Traverse** - 1/40th of transect counted  
3.  **Eyecount** - full transect counted

| Sheet Number | Description                                   | Units                |
|--------------|-----------------------------------------------|----------------------|
| Sheet 1      | Phytoplankton taxa densities by silk transect | Abundance / Transect |
| Sheet 2      | Zooplankton taxa densities by silk transect   | Abundance / Transect |
| Sheet 3      | Eyecount taxa densities by silk transect      | Abundance / Transect |

Conversions to a standard unit of measurement is necessary when working
together with these two sources.
