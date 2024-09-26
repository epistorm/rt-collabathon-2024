
<!-- README.md is generated from README.Rmd. Please edit that file -->

# summrt

<!-- badges: start -->

[![R-CMD-check](https://github.com/EpiForeSITE/summrt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EpiForeSITE/summrt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `summrt` is to create wrapper functions around the outputs
of common R(t) esitmation packages, in order to facilitate comparison of
outputs. While many R(t) estimation packages provide numerous outputs,
we start by consolidating outputs to a standardized time indexing
(integer days) and reporting out a median and 95% confidence interval on
R(t), indexed starting on the first day of reported data.

## Installation

You can install the development version of summrt from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EpiForeSITE/summrt")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(summrt)
## read data
rtestim_obj <- readRDS(
  system.file("extdata", "rtestim_example.rds", package = "summrt")
)

## standardize output from rtestim 
std_rtestim <- summarize_rtestimate(rtestim_obj, lambda = "lambda.min")
std_rtestim
#> Summary of Rt estimation
#> Package :  rtestim 
#> Notes   :  cv_poisson_rt 
#> # A tibble: 61 × 4
#>     date median     lb    ub
#>    <int>  <dbl>  <dbl> <dbl>
#>  1     0  1.01  0       2.24
#>  2     1  0.885 0       1.80
#>  3     2  0.847 0.0994  1.59
#>  4     3  0.869 0.183   1.55
#>  5     4  0.934 0.223   1.64
#>  6     5  1.03  0.258   1.80
#>  7     6  1.13  0.326   1.94
#>  8     7  1.22  0.402   2.03
#>  9     8  1.28  0.486   2.07
#> 10     9  1.32  0.560   2.08
#> # ℹ 51 more rows
```
