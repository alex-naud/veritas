
<!-- README.md is generated from README.Rmd. Please edit that file -->

# veritas

<!-- badges: start -->
<!-- badges: end -->

This package provide a set of functions to with datasets generated from
the veritas questionnaire. See [Naud et al.,
2020](https://doi.org/10.1016/j.healthplace.2020.102454)

## Installation

You can install the the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alex-naud/veritas-r", build_vignettes = TRUE)
```

## Load datasets

``` r
veritas_data <- transformData(locations,
                              people,
                              groups,
                              relations)
```

## Create social networks

``` r
social_networks <- createSocialNetworks(veritas_data)
```

## Calculate social network measures

``` r
# Calculate base social network indicator
sn_measures <- socialNetworkMeasures(social_networks)

# Calcul SN measures specific to InterAct project
interact_sn_measures <- interactSnMeasures(veritas_data)

# Combine both data frames
sn_measures <- merge(base_sn_measures, interact_sn_measures, 
                     by = "pid")
```

## Further information

For a detail presentation, see the introduction vignette

``` r
browseVignettes("veritas")
```
