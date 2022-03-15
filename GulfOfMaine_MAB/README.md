
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Gulf of Maine CPR Transect Data (MBA Sourced)

## About:

This folder contains documentation on the Gulf of Maine CPR data
obtained directly from the Marine Biological Association (MBA), and
adjustments made prior to its integration into ERDDAP.

## MBA/SAHFOS Gulf of Maine CPR Data

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

#### Data Processing Code

## ERDDAP Metadata
