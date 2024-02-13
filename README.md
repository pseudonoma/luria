
<!-- README.md is generated from README.Rmd. Please edit that file -->

# luria

<!-- badges: start -->
<!-- badges: end -->

This package provides a general framework and data analysis pipeline for
conducting fluctuation analysis. Its functions are built around a
modification of the fluxxer.R script written by J. Barrick & D.
Deatherage, which in turn calls functions from the rSalvador package to
produce mutation rate estimates and comparisons.

## Installation

To install the package, run the following lines:

``` r
# install.packages("devtools")
devtools::install_github("pseudonoma/luria")
```

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(luria)
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
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
