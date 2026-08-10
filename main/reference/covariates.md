# Extract Covariate Names

Extract Covariate Names

## Usage

``` r
covariates(object, ...)
```

## Arguments

- object:

  (`ANY`) the object to extract covariate names from.

- ...:

  additional arguments added by methods.

## Value

A character vector containing the covariate names.

## See also

Other covariates:
[`covariates.DataSurvival()`](https://genentech.github.io/jmpost/reference/covariates.DataSurvival.md)

## Examples

``` r
surv_data <- DataSurvival(os_data, Surv(os_time, os_event) ~ age + sex)
covariates(surv_data)
#> [1] "age"  "sexM"
```
