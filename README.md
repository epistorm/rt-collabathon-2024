
<!-- README.md is generated from README.Rmd. Please edit that file -->

# summrt

<!-- badges: start -->

[![R-CMD-check](https://github.com/EpiForeSITE/summrt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EpiForeSITE/summrt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `summrt` is to create wrapper functions around the outputs of common R(t) esitmation packages, in order to facilitate comparison of outputs.
While many R(t) estimation packages provide numerous outputs, we start by consolidating outputs to a standardized time indexing (integer days) and
reporting out a median and 95% confidence interval on R(t), indexed starting on the first day of reported data. 

Packages this currently supports:
- [EpiNow2](https://github.com/epiforecasts/EpiNow2)
- [EpiEstim](https://github.com/mrc-ide/EpiEstim)
- [EpiLPS](https://github.com/oswaldogressani/EpiLPS)
- [rtestim](https://github.com/dajmcdon/rtestim)

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
## basic example code
```
