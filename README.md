# mt.surv <img src="man/figures/mt.surv_logo.png" align="right" width="100" />

## Multi-threshold survival analysis 

[![DOI](https://zenodo.org/badge/476856711.svg)](https://zenodo.org/badge/latestdoi/476856711)    


## Overview

This package contains functions to apply multi-threshold survival analysis on microbiome data, using (1) survival information and (2) microbial reads

`survivalByQuantile` generates a table with cox analysis parameters at various threshold quantile

`calculateArea` quantify how sensitive a taxonomy level is to threshold quantile

`ToWide` convert the microbe reads from long format to wide format


## Installation

``` r
# Currently available as the development version on GitHub
install.packages("devtools")
devtools::install_github("spakowiczlab/mt.surv")
```

## References

1. Andersen, P. and Gill, R. (1982). Cox's regression model for counting processes, a large sample study. Annals of Statistics 10, 1100-1120.

2. Therneau, T., Grambsch, P., Modeling Survival Data: Extending the Cox Model. Springer-Verlag, 2000.

_submitted_
