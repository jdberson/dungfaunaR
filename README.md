
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Quick guide

Once you have cloned the repository to your computer, open the project
and run:
```
devtools::install()
```
This will install the package to your local library.

The data to use for the dashboard is:
```
data(“dungfauna_aus”)
```

These data contain records from DBEE, QLD and a WA project. Not for
public release just yet.

To view an interactive visualisation of the data, run
```
runShinyApp()
```
If the app window doesn't start automatically, navigate to the port printed to
your console (usually [localhost:5197](localhost:5197)) in your web browser.


There will be some further refinements to these data but I think we are
at a stage to include a dashboard in the package.

So, the aim is to:

# - 1.  incorporate a copy of the dashboard into the package so that when
#       a user installs the package they can run the dashboard locally.
# 
# - 2.  update the dashboard so that it includes
# 
# - all species found in the scientificName column (there are now new
#   species from the QLD and WA projects that will need to be added to the
#   ‘Select a species’ menu)

# - A new menu item allowing for data to be filtered by datasetName
#   (i.e. project)

# - Remove the prediction tab

# - Put back in the data tab

- Turn off the jittering of points

- Any other minor changes we think of

**Note: this is not a replacement of the existing dashboard but a
copy.**

# dungfaunaR

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

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dungfaunaR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
