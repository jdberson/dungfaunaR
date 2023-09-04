
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dungfaunaR

## Overview

This is a data package with data on occurrence and abundance records for
deliberately introduced dung beetles in Australia. Currently, the data
contain 1,752,807 species identifications from 22,718 occurrence
records, taken from 10,272 sampling events at 546 locations. The data
also contain 213,538 absence records. The data are explained in detail
in Berson et al (submitted).

Code used to format the data can be found in the `data-raw folder` in
the [dungfaunaR GitHub repository](link%20here). Note that we formatted
and performed checks on data from different projects separately (see
`data-raw/qld_2001_2010.R`, `data-raw/dafwa_wa_2012_2014.R` and
`data-raw/dbee_2019_2022.R`), before combining these data into one
dataset (see `data-raw/dungfauna_aus.R`).

Code used to generate the summary statistics, figures and tables in
Berson et al (submitted) can by found in the `data-paper` folder within
the `data-raw` folder.

The data are provided in both wide (`dungfauna_event`) and long
(`dungfauna_occurrence`) format. Running `dungfaunaR::runShinyApp()`
will launch a shiny app for visually exploring the data.

## Installation

You can install the development version of dungfaunaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jdberson/dungfaunaR")
```

## Getting started

After installing the package, the following code load the data into your
R session:

``` r
library(dungfaunaR)

# To load the data in wide format:
data("dungfauna_event")

# To load the data in long format:
data("dungfauna_occurrence")
```

After installing and loading the package, you can explore the data with
the built-in shiny app using `dungfaunaR::runShinyApp()`

## Citation

The paper detailing the data is currently submitted as a data paper.
Please check back here for the correct citation to use when using the
data.

## Issues

If you find an error or a bug we would love to hear from you! Please let
us know what you have found by creating an issue at
<https://github.com/jdberson/dungfaunaR/issues>.
