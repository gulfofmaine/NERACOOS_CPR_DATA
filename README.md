
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

<div id="daemdlqrcb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#daemdlqrcb .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#daemdlqrcb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#daemdlqrcb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#daemdlqrcb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#daemdlqrcb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daemdlqrcb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#daemdlqrcb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#daemdlqrcb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#daemdlqrcb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#daemdlqrcb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#daemdlqrcb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#daemdlqrcb .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#daemdlqrcb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#daemdlqrcb .gt_from_md > :first-child {
  margin-top: 0;
}

#daemdlqrcb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#daemdlqrcb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#daemdlqrcb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#daemdlqrcb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#daemdlqrcb .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#daemdlqrcb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#daemdlqrcb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#daemdlqrcb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#daemdlqrcb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#daemdlqrcb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#daemdlqrcb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#daemdlqrcb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#daemdlqrcb .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#daemdlqrcb .gt_left {
  text-align: left;
}

#daemdlqrcb .gt_center {
  text-align: center;
}

#daemdlqrcb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#daemdlqrcb .gt_font_normal {
  font-weight: normal;
}

#daemdlqrcb .gt_font_bold {
  font-weight: bold;
}

#daemdlqrcb .gt_font_italic {
  font-style: italic;
}

#daemdlqrcb .gt_super {
  font-size: 65%;
}

#daemdlqrcb .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Sheet Number</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Description</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Units</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Sheet 1</td>
<td class="gt_row gt_left">Phytoplankton taxa densities by silk transect</td>
<td class="gt_row gt_left">Abundance / 1m3</td></tr>
    <tr><td class="gt_row gt_left">Sheet 2</td>
<td class="gt_row gt_left">Zooplankton taxa densities by silk transect</td>
<td class="gt_row gt_left">Abundance / 100m3</td></tr>
  </tbody>
  
  
</table>
</div>

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

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

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

<div id="ccgwarkhql" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ccgwarkhql .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ccgwarkhql .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ccgwarkhql .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ccgwarkhql .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ccgwarkhql .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ccgwarkhql .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ccgwarkhql .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ccgwarkhql .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ccgwarkhql .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ccgwarkhql .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ccgwarkhql .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ccgwarkhql .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ccgwarkhql .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ccgwarkhql .gt_from_md > :first-child {
  margin-top: 0;
}

#ccgwarkhql .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ccgwarkhql .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ccgwarkhql .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ccgwarkhql .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ccgwarkhql .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ccgwarkhql .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ccgwarkhql .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ccgwarkhql .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ccgwarkhql .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ccgwarkhql .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ccgwarkhql .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ccgwarkhql .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ccgwarkhql .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ccgwarkhql .gt_left {
  text-align: left;
}

#ccgwarkhql .gt_center {
  text-align: center;
}

#ccgwarkhql .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ccgwarkhql .gt_font_normal {
  font-weight: normal;
}

#ccgwarkhql .gt_font_bold {
  font-weight: bold;
}

#ccgwarkhql .gt_font_italic {
  font-style: italic;
}

#ccgwarkhql .gt_super {
  font-size: 65%;
}

#ccgwarkhql .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Sheet Number</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Description</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Units</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Sheet 1</td>
<td class="gt_row gt_left">Phytoplankton taxa densities by silk transect</td>
<td class="gt_row gt_left">Abundance / Transect</td></tr>
    <tr><td class="gt_row gt_left">Sheet 2</td>
<td class="gt_row gt_left">Zooplankton taxa densities by silk transect</td>
<td class="gt_row gt_left">Abundance / Transect</td></tr>
    <tr><td class="gt_row gt_left">Sheet 3</td>
<td class="gt_row gt_left">Eyecount taxa densities by silk transect</td>
<td class="gt_row gt_left">Abundance / Transect</td></tr>
  </tbody>
  
  
</table>
</div>

Conversions to a standard unit of measurement is necessary when working
together with these two sources.
