
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rTRIPLEXCWFlux

<!-- badges: start -->

[![R-CMD-check](https://github.com/ShulanSun/rTRIPLEX_CW_Flux/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ShulanSun/rTRIPLEX_CW_Flux/actions/workflows/R-CMD-check.yaml)
[![Travis build
status](https://travis-ci.com/ShulanSun/rTRIPLEX_CW_Flux.svg?branch=main)](https://travis-ci.com/ShulanSun/rTRIPLEX_CW_Flux)
<!-- badges: end -->

## Purpose

The `rTRIPLEXCWFlux` package encodes the carbon uptake submodule and
evapotranspiration submodule of the `TRIPLEX-CW-Flux` model to run the
simulation of carbon-water coupling. `TRIPLEX-CW-Flux` model is based on
two well-established models, `TRIPLEX-Flux` model ([Zhou et al.,
2008](https://doi.org/10.1016/j.ecolmodel.2008.07.011)) and
`Penman–Monteith` model ([Monteith,
1965](https://www.semanticscholar.org/paper/Evaporation-and-environment.-Monteith/428f880c29b7af69e305a2bf73e425dfb9d14ec8)).
In the R script, the user only needs to download and load the
`rTRIPLEX-CW-Flux` package, and then input the variables and parameters
of the model to obtain simulated results. This package accelerates the
running speed of the model and facilitates the estimation of carbon
sequestration and water consumption in different forest ecosystems using
extensive flux observed data.This could make `TRIPLEX-CW-Flux` model
free available and more popular in future study.

## Installation

You can install the development version of rTRIPLEXCWFlux from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github(repo="ShulanSun/rTRIPLEX_CW_Flux")
```

## Usage

This is a basic example which shows you how to run the package. In this
example, we run a simulation for a Chinese fir plantation. The input
data are provided as internal data in `rTRIPLEXCWFlux`. The outputs of
the `TRIPLEX_CW_Flux` function are a long format dataframe and charts of
simulated result for net ecosystem productivity (NEP) and
evapotranspiration (ET) at 30 min scale, and monthly variation of the
input environmental factors.

``` r
library(rTRIPLEXCWFlux)
out<-TRIPLEX_CW_Flux (Input_variable=Inputvariable,
                 Input_parameter=Inputpara,
                 overyear = TRUE)
head(out)
## basic example code
```

## Author

Shulan Sun, Wenhua Xiang, Shuai Ouyang, Xiaolu Zhou, Changhui Peng

## Citation

Evaporation and Environment. Symposia of the Society for Experimental
Biology, 19, 205-234.
[https://www.semanticscholar.org/paper/Evaporation-and-environment.-Monteith](https://www.semanticscholar.org/paper/Evaporation-and-environment.-Monteith/428f880c29b7af69e305a2bf73e425dfb9d14ec8)

Zhou, X.L., Peng, C.H., Dang, Q.L., Sun, J.F., Wu, H.B., &Hua, D.
(2008). Simulating carbon exchange in Canadian Boreal forests: I. Model
structure, validation, and sensitivity analysis. Ecological
Modelling,219(3-4), 287-299.
<https://doi.org/10.1016/j.ecolmodel.2008.07.011>
