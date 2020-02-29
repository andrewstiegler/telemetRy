telemetRy: for interacting with DSI telemetry data in R
================
Andrew Stiegler
2/28/2020

## Description

This is a package for interacting with telemetry data using R. Specific
functions for importing data from DSI telemetry systems are included, as
well as functions for analyzing telemetry data over various timescales.

Note: the author of this package has no affiliation with
[DSI](https://www.datasci.com)

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

    ##       .id                Time TimesOnly ElapsedTime ElapsedTimeMin ElapsedTimeH
    ## 1 1024071 2019-12-11 06:00:00  06:00:00           0      0.0000000  0.000000000
    ## 2 1024071 2019-12-11 06:00:10  06:00:10          10      0.1666667  0.002777778
    ## 3 1024071 2019-12-11 06:00:20  06:00:20          20      0.3333333  0.005555556
    ## 4 1024071 2019-12-11 06:00:30  06:00:30          30      0.5000000  0.008333333
    ## 5 1024071 2019-12-11 06:00:40  06:00:40          40      0.6666667  0.011111111
    ## 6 1024071 2019-12-11 06:00:50  06:00:50          50      0.8333333  0.013888889
    ##   ElapsedTimeD      SBP      DBP      MAP       HR     Temp     Activity
    ## 1 0.0000000000 110.6964 79.31778 89.77731 462.3699 34.33864 0.0009920789
    ## 2 0.0001157407 110.2798 78.98634 89.41750 474.1897 34.34655 0.0009920789
    ## 3 0.0002314815 110.4358 78.48251 89.13360 463.0186 34.35817 0.0009920789
    ## 4 0.0003472222 109.3194 78.21652 88.58413 483.5821 34.36127 0.0009920789
    ## 5 0.0004629630 114.4518 83.79416 94.01337 510.7150 34.37057 0.0009920789
    ## 6 0.0005787037 108.3751 76.58762 87.18345 555.7769 34.37879 0.0009920789

Where the .id column indicates the subject serial number, and the Time
column is the real time of data acquisition. Several useful time columns
are generated, including a column for time-of-day only, and elapsed time
since acquisition started in several units. The rest of the telemetry
parameters are named.

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
export_data_sbp <- isolate_sbp(data = sample_BP_data)
export_data_sbp
```

    ##                   Time 1024071SBP 1024084SBP 1033468SBP 1033470SBP 1033478SBP
    ## 1  2019-12-11 06:00:00   110.6964   109.4992   122.9087   104.9790   115.3066
    ## 2  2019-12-11 06:00:10   110.2798   109.7520   119.2668   105.1184   116.1182
    ## 3  2019-12-11 06:00:20   110.4358   109.9445   115.7024   101.1247   114.1084
    ## 4  2019-12-11 06:00:30   109.3194   109.6939   113.6496   100.9263   113.7861
    ## 5  2019-12-11 06:00:40   114.4518   108.4125   110.8669   105.2866   114.1150
    ## 6  2019-12-11 06:00:50   108.3751   108.2852   111.0773   109.4639   113.8400
    ## 7  2019-12-11 06:01:00   111.7881   107.9619   108.9074   109.4410   113.5838
    ## 8  2019-12-11 06:01:10   113.0239   107.8853   107.5131   105.7928   114.6587
    ## 9  2019-12-11 06:01:20   113.7833   104.6345   107.9636   100.2636   108.9264
    ## 10 2019-12-11 06:01:30   113.1145   107.3783   107.8785   106.2690   116.6602
    ##    1033480SBP 1037725SBP 1037726SBP 1037727SBP 1037728SBP 1080808SBP 814438SBP
    ## 1    130.6973   95.13095   150.3063   125.1629   118.1151   116.9220  117.1178
    ## 2    130.6173   95.52164   141.3600   122.8405   116.8214   114.7564  116.8509
    ## 3    131.7799   95.98300   133.1895   124.7566   119.5491   114.8461  116.7804
    ## 4    130.8443   97.48119   132.7130   128.7910   118.2141   113.7573  115.6282
    ## 5    129.0964  102.29418   132.4251   126.8370   118.9545   112.6140  116.0752
    ## 6    133.5518  101.79295   128.2162   126.3668   121.2308   110.2732  115.7999
    ## 7    131.0404   98.98191   125.1488   125.9093   122.1842   110.5929  115.0460
    ## 8    135.1047   97.99588   125.8021   131.9214   119.5645   108.5979  114.1832
    ## 9    137.0700   98.39948   123.1397   125.8221   121.6235   109.8495  114.9082
    ## 10   136.0600  101.17905   123.6936   129.3062   124.6963   109.0615  115.2644
    ##    848072SBP 865262SBP 967522SBP     mean elapsed_time
    ## 1   112.4122  112.9085  110.3607 116.8349            0
    ## 2   116.2459  110.3665  108.4577 115.6249           10
    ## 3   121.7860  111.2077  109.0263 115.3480           20
    ## 4   127.0519  115.6383  109.1869 115.7788           30
    ## 5   120.9002  115.4543  112.1568 115.9960           40
    ## 6   117.9933  113.8289  111.1773 115.4182           50
    ## 7   114.0924  113.6590  109.1996 114.5024           60
    ## 8   117.7039  111.7958  110.3915 114.7956           70
    ## 9   118.4992  112.3842  107.4810 113.6499           80
    ## 10  122.1034  113.1061  110.0623 115.7222           90

``` r
# To isolate SBP from a typical_day
typical_day_sbp <- typical_sbp(data = typical_day_output)
```
