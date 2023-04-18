
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Quick guide

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dungfaunaR)](https://CRAN.R-project.org/package=dungfaunaR)
[![Codecov test
coverage](https://codecov.io/gh/jdberson/dungfaunaR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jdberson/dungfaunaR?branch=master)
<!-- badges: end -->

The goal of dungfaunaR is to …

## Installation

You can install the development version of dungfaunaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jdberson/dungfaunaR")
```

## Load the data in R

To load the data in R, run

``` r
library(dungfaunaR)
data('dungfauna_aus')
```

## View data interactively

To view an interactive visualisation of the data, run

``` r
runShinyApp()
```

If the app window doesn’t start automatically, navigate to the port
printed to your console (usually [localhost:5197](localhost:5197)) in
your web browser.

## Install and run from source

To install and run from source, clone this repository and, from R, run

``` r
devtools::load_all()
```
