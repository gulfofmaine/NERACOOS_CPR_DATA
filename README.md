
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

#### Data Acquisition:

Data obtained from NOAA/NEFSC spans the period of: 1961-2013, and was
delivered in the following excel file:

> **data_raw/NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014
> update).xlsx**

#### Data Organization:

That excel file is divided into 2 sheets. One contains data on
phytoplankton and the other contains records on zooplankton. Each excel
sheet has two or more additional rows in the header explaining the
identification and development stage of certain taxa.

**NOTE: CPR data from NOAA is reported in units that do not follow CPR
survey conventions. (ref: Jossi et al. 2003)**

| Sheet Number | Description                                   | Units             |
|--------------|-----------------------------------------------|-------------------|
| Sheet 1      | Phytoplankton taxa densities by silk transect | Abundance / 1m3   |
| Sheet 2      | Zooplankton taxa densities by silk transect   | Abundance / 100m3 |

#### Data Processing Code

To make the CPR data more “ERDDAP-friendly” minor changes were applied
to reshape the structure of the original dataset, resulting in a longer
& [tidy-er dataset](https://r4ds.had.co.nz/tidy-data.html). Any
modifications to the original files have been documented in the R
scripts held in the [R/](www.github.com/gulfofmaine/R) sub-directory and
more explicitly detailed in the steps of `_targets.R`.

Taxonomic encodings from the original excel file headers are also
checked against MARMAP codes to ensure accuracy
<https://www.nefsc.noaa.gov/nefsc/Narragansett/taxcodesA.html>

#### Reproducing the Reformatting of CPR Data

The full processing pipeline from raw data to its ERDDAP format has been
implemented using the [{targets}](https://docs.ropensci.org/targets/)
R-package, and can be recreated in full by running the following code in
an active R session. (Assuming all R-packages are installed).

    library(targets)
    tar_make()

This will recreate the processing steps outlined in `_targets.R`:

<img src="README_files/figure-gfm/gulf of maine pipeline-1.png" width="100%" />

## ERDDAP Metadata

To make the datasets available on ERDDAP, appropriate metadata had to be
detailed as a .xml file: `erddap_xml/dataset.xml`. This file populates
the metadata information for the two datasets [zooplankton
dataset_id](http://ismn.erddap.neracoos.org/erddap/tabledap/noaa_gom_traverse.html)
& [phytoplankton
dataset_id](http://ismn.erddap.neracoos.org/erddap/tabledap/noaa_gom_phyto.html).
The information in the `datasets.xml` file is displayed online via the
erddap interface, and is what allows ERDDAP to process data filtering
services when accessing/downloading the data.

An additional pdf with history on the survey and key references is also
available in `refs/gom_cpr_metadata.pdf`. This folder also contains
additional references on the history of the survey.

------------------------------------------------------------------------

## Working with CPR Data

Data can be accessed directly from
[ERDDAP](http://ismn.erddap.neracoos.org/erddap/index.html), or through
software packages like R’s
{[rerddap](https://github.com/ropensci/rerddap/tree/0723cafce46cf31dd46efed71d76706788c08889)}.

``` r
# Accessing the Gulf of Maine CPR Zooplankton Data
library(rerddap)

# Get the tabledap information from the server link and dataset_id
cpr_info <- info(url = "http://ismn.erddap.neracoos.org/erddap", datasetid = "noaa_gom_traverse")

# Use the tabledap function to import all the data (optionally add filters)
gom_zooplankton <- tabledap(cpr_info)
```

Or python’s {[erddapy](https://ioos.github.io/erddapy/)}.

``` python
# Accessing the Gulf of Maine CPR Zooplankton Data
from erddapy import ERDDAP
e = ERDDAP(
  server = "http://ismn.erddap.neracoos.org/erddap",
  protocol = "tabledap",
  response = "csv"
)

# Add Dataset ID
e.dataset_id = "noaa_gom_phyto"

# Coerce to pandas df
gom_phytoplankton = e.to_pandas()
```

------------------------------------------------------------------------

## SAHFOS Gulf of Maine CPR Data

In addition to the data obtained from NOAA, an additional 4-years of CPR
data was obtained from [the Sir Alister Hardy
Foundation](https://www.cprsurvey.org/about-us/sir-alister-hardy-and-the-continuous-plankton-recorder-cpr-survey/)
(now the [Marine Biological
Association](https://www.cprsurvey.org/about-us/the-marine-biological-association/)).

#### Data Acquisition:

Data obtained from SAHFOS spans the period of: **2013-2017**, and was
delivered in the following files:

> **data_raw/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx**  
> **data_raw/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx**

#### Data Organization:

Each of these files contain **three** sheets containing the different
counting scales of the CPR survey, the phytoplankton, the traverse, and
the “eye-count” scales. These three scales are based on the size of
organisms counted. At each scale different subsets of the silk transect
are used when counting the individually identified taxa. Counts from the
subsets are then scaled to the entire silk transect and given a number
on a categorical counting scale. These represent discrete jumps in
abundance per transect.

These three measurement increments correspond with the following
sub-sampling protocols to save time when counting very small organisms
(ref: Warner & Hays 1995):

1.  **Phyto** - 1/8000th of transect counted  
2.  **Traverse** - 1/40th of transect counted  
3.  **Eyecount** - full transect counted

| Sheet Number | Description                                   | Units                |
|--------------|-----------------------------------------------|----------------------|
| Sheet 1      | Phytoplankton taxa densities by silk transect | Abundance / Transect |
| Sheet 2      | Zooplankton taxa densities by silk transect   | Abundance / Transect |
| Sheet 3      | Eyecount taxa densities by silk transect      | Abundance / Transect |

#### Reconciling Unit and Taxa Classification Differences

Conversions to a standard unit of measurement is necessary when working
together with these two sources. Additionally, the taxa and group stages
are also inconsistent and require a key for transitioning to coarser
scale groups.
