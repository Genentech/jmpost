# `SurvivalWeibullPH`

This class extends the general
[`SurvivalModel`](https://genentech.github.io/jmpost/reference/SurvivalModel-class.md)
class for using the Weibull proportional hazards survival model.

## Usage

``` r
SurvivalWeibullPH(
  lambda = prior_gamma(2, 0.5),
  gamma = prior_gamma(2, 0.5),
  beta = prior_normal(0, 2)
)
```

## Arguments

- lambda:

  (`Prior`) for the scale `lambda`.

- gamma:

  (`Prior`) for the shape `gamma`.

- beta:

  (`Prior`) for covariates coefficients `beta`.

## Value

A `SurvivalWeibullPH` object.

## Examples

``` r
SurvivalWeibullPH()
#> 
#> Weibull-PH Survival Model with parameters:
#>     sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
#>     sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
#>     beta_os_cov ~ normal(mu = 0, sigma = 2)
#> 
```
