
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NERACOOS_CPR_DATA

Project to integrate Continuous Plankton Recorder data into NERACOOS &
ERDDAP

This repository contains the necessary code and documentation to support
hosting the [Gulf of Maine Continuous Plankton Recorder Survey
Data](https://www.fisheries.noaa.gov/feature-story/long-running-plankton-survey-resume-gulf-maine)
on
[ERDDAP](http://ismn.erddap.neracoos.org/erddap/info/index.html?page=1&itemsPerPage=1000).

## Organization:

This repository documents the data provenance for continuous plankton
recorder data obtained from a number of scientific research agencies
(NOAA and MAB), and covering different sampling transects (The Gulf of
Maine & The Mid-Atlantic Bight Transects).

Raw data from all sources is contained in the `data_raw/` directory.
Code that prepares the raw data for ERDDAP and any necessary
documentation is specific to the source that the data was recieved from.
This information can be found in the following sub-folders:

| Sub-Folder       | Description                                    |
|------------------|------------------------------------------------|
| GulfOfMaine_NOAA | Gulf of Maine CPR Data obtained from NOAA      |
| GulfOfMaine_MAB  | Gulf of Maine CPR Data Obtained from MAB       |
| MidAtlantic_NOAA | Mid-Atlantic Bight CPR Data Obtained from NOAA |
| MidAtlantic_MAB  | Mid-Atlantic Bight CPR Data Obtained from MAB  |

These resources have been processed independently due to differences in
measurement units and organization structures. Documentation on how each
dataset was received and treated prior to uploading into ERDDAP is
documented within each of the corresponding sub-folders.

### Reproducing the Data Transformations

The full processing pipeline from the raw data to their final ERDDAP
formats has been implemented using the
[{targets}](https://docs.ropensci.org/targets/) R-package, and can be
recreated in full by running the following code in an active R session.
(Assuming all R-packages are installed).

    library(targets)
    tar_make()

This will recreate the processing steps outlined in `_targets.R` that
transform the raw files into the format uploaded onto ERDDAP:

<img src="README_files/figure-gfm/gulf of maine pipeline-1.png" width="100%" />

### Reconciling Unit and Taxa Classification Differences

Conversions to a standard unit of measurement is necessary when working
together with CPR obtained from these two sources. Another additional
hurdle is that the taxa and group stages are also inconsistently used
across them, and require a key for transitioning to coarser scale
groups.

Information on resolving the differences between these two data
resources can be found in the following sub folder:
`working_across_sources/`, with examples of code working from ERDDAP as
a starting point.
