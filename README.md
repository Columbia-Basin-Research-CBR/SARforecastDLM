
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SARforecastDLM

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<!-- lastcommit: start -->

[![Last
Commit](https://img.shields.io/github/last-commit/Columbia-Basin-Research-CBR/SARforecastDLM)](https://github.com/Columbia-Basin-Research-CBR/SARforecastDLM/commits/main)
<!-- lastcommit: end -->

**SARforecastDLM** uses Dynamic Linear Modelling (DLM) to forecast
changes in one-year ocean survival of wild spring/summer Snake River
Chinook salmon, *Oncorhynchus tshawytscha*, from [upwelling indices
(CUI)](https://oceanview.pfeg.noaa.gov/products/upwelling/intro), West
Coast, USA. This ShinyApp is developed based on the [Scheuerell and
Williams
(2005)](https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2419.2005.00346.x).

## Installation

You can install the development version of **SARforecastDLM** from
[GitHub](https://github.com/) with:

``` r
# Install the developmental version of the HydroSurvSizePred package from GitHub
devtools::install_github("Columbia-Basin-Research-CBR/SARforecastDLM")

# Load the package
library(SARforecastDLM)

# Run the app
run_app()
```

**SARforecastDLM** is currently in development and changes are
continuously being made. If you have already imported to R studio,
please rerun
`install_github("Columbia-Basin-Research-CBR/SARforecastDLM")` to see
latest changes in the developmental version. If no changes have been
made since last import, a warning will appear:
`Skipping install of 'SARforecastDLM' from a github remote, the SHA1 (fdd71350) has not changed since last install. Use 'force = TRUE' to force installation`,
indicating you have the latest developmental version imported.

If you are interested in the files that support the development version,
please see:
<https://github.com/Columbia-Basin-Research-CBR/SARforecastDLM> for
files necessary to run.

The app structure follows a Golem framework described in [Engineering
Production-Grade Shiny
Apps](https://engineering-shiny.org/setting-up-for-success.html) by
Colin Fay, Sébastien Rochette, Vincent Guyader and Cervan Girard.

## Contact

This app is being developed by Caitlin O’Brien, Research Scientist,
Columbia Basin Research, SAFS, University of Washington. Please reach
out with questions/concerns via <csobrien@uw.edu>.