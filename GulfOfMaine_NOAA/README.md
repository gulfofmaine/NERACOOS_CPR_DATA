
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Gulf of Maine CPR Transect Data (NOAA Sourced)

## About:

This folder contains documentation on the Gulf of Maine CPR data
obtained directly from NOAA, and adjustments made prior to its
integration into ERDDAP.

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
cpr_info <- info(url = "http://ismn.erddap.neracoos.org/erddap", 
                 datasetid = "noaa_gom_cpr_zooplankton")

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
e.dataset_id = "noaa_gom_cpr_phytoplankton"

# Coerce to pandas df
gom_phytoplankton = e.to_pandas()
```
