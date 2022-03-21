
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NERACOOS_CPR_DATA

Integrating Continuous Plankton Recorder data into NERACOOS & ERDDAP

This repository contains the necessary code and documentation to support
hosting the [Gulf of Maine Continuous Plankton Recorder Survey
Data](https://www.fisheries.noaa.gov/feature-story/long-running-plankton-survey-resume-gulf-maine)
on
[ERDDAP](http://ismn.erddap.neracoos.org/erddap/info/index.html?page=1&itemsPerPage=1000).

## Organization:

This repository documents the data provenance for continuous plankton
recorder data obtained from a number of scientific research agencies
([NOAA](https://www.fisheries.noaa.gov/about/northeast-fisheries-science-center)
and [MAB](https://www.mba.ac.uk/)), and covering different sampling
transects (The Gulf of Maine & The Mid-Atlantic Bight Transects).

Raw data from all sources is contained in the `data_raw/` directory.
Code that prepares the raw data for ERDDAP and any necessary
documentation is specific to the source that the data was received from.
This information can be found in the following sub-folders:

| Sub-Folder       | Description                                    |
|------------------|------------------------------------------------|
| GulfOfMaine_NOAA | Gulf of Maine CPR Data obtained from NOAA      |
| GulfOfMaine_MBA  | Gulf of Maine CPR Data Obtained from MBA       |
| MidAtlantic_NOAA | Mid-Atlantic Bight CPR Data Obtained from NOAA |
| MidAtlantic_MBA  | Mid-Atlantic Bight CPR Data Obtained from MBA  |

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

The DAG above shows a simplified representation of the steps for the
NOAA ZPR Zooplankton data, where the taxonomic information found in the
header is separated from the abundance information and later joined back
after it has been reshaped. Similar cleanup paths exist for the data
obtained from NOAA as well as the data obtained from the MBA.

### Unit and Taxa Classification Differences

Due to how the CPR data is stored and maintained within these two
institutions, conversions to a standard unit of measurement is necessary
when working with CPR jointly from both sources. In addition to unit
conversions, there are taxonomic and development stages that are
recorded inconsistently across them, which require the use of a key for
transitioning to coarser scale groupings.

Information on resolving the differences between these two data
resources can be found in the following sub folder:
`working_across_sources/`, with examples of code working from ERDDAP as
a starting point.

# Project Funding:

Funding for making these resources available was provided through grant
awards from the [National Science Foundation](https://www.nsf.gov/) and
from the [Lenfest Ocean Program](https://www.lenfestocean.org/en). With
communication and support from the Northeast Fisheries Science Center
and the Marine Biological Association.

------------------------------------------------------------------------

## Bonus Resources:
