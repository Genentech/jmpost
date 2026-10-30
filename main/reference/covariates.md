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

For
[`DataSurvival`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md),
a character vector containing the covariate names. For
[`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md),
a named list of character vectors, one per covariate coefficient
parameter; see
[`covariates.LongitudinalModel()`](https://genentech.github.io/jmpost/reference/covariates.LongitudinalModel.md).

## See also

Other covariates:
[`covariates.DataSurvival()`](https://genentech.github.io/jmpost/reference/covariates.DataSurvival.md),
[`covariates.LongitudinalModel()`](https://genentech.github.io/jmpost/reference/covariates.LongitudinalModel.md)

## Examples

``` r
surv_data <- DataSurvival(os_data, Surv(os_time, os_event) ~ age + sex)
covariates(surv_data)
#> [1] "age"  "sexM"
```
