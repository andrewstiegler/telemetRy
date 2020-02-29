telemetRy: for interacting with DSI telemetry data in R
================
Andrew Stiegler
2/28/2020

## Description

This is a package for interacting with telemetry data using R. Specific
functions for importing data from DSI telemetry systems are included, as
well as functions for analyzing telemetry data over various timescales.

## Installation

telemetRy is available on github. To install telemetRy, you’ll need:  
[R \> 3](https://www.r-project.org "R \> 3.4")  
[devtools](https://github.com/r-lib/devtoolsdevtools "devtools") package
from Hadley Wickham

First, install devtools:

``` r
install.packages('devtools')
```

Then install telemetRy:

``` r
devtools::install_github(repo = 'andrewstiegler/telemetRy')
library(telemetRy)
```

## Importing DSI data

First data must be exported from DSI’s Ponemah software.

With an experiment open, navigate to Experiment \> Export Data. In this
pop-up, select subjects to export, a timerange to export, and make sure
to un-check “Pivot Compatible Sheets”

![Screenshot of DSI Ponemah export
screen](images/ponemah_export_pivot.png)

Select “Export” and an Excel file will be generated. To import that
Excel file into R, utilize the DSI\_export\_to\_dataframe function:

``` r
exported_data <- DSI_export_to_dataframe('path_to_DSI_export')
```

The export will be a dataframe:

## Calculating typical day from DSI export

After importing into R, pass the imported dataframe to the typical\_day
function, and specify the beginning of the light cycle (in 24H).

``` r
# For example, room lights turn on at 6AM
typical_day_output <- typical_day(data = exported_data, lights_on = 6)
```

## Other useful functions

Several functions are available for isolating BP parameters from
datasets. Separate functions exist for the entire dataset or for
typical\_day averages.

``` r
# For example, to isolate SBP from entire dataset
export_data_sbp <- isolate_sbp(data = exported_data)

# To isolate SBP from a typical_day
typical_day_sbp <- typical_sbp(data = typical_day_output)
```
