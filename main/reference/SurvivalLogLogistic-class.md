# `SurvivalLogLogistic`

This class extends the general
[`SurvivalModel`](https://genentech.github.io/jmpost/reference/SurvivalModel-class.md)
class for using the log-logistic survival model.

## Usage

``` r
SurvivalLogLogistic(
  a = prior_lognormal(log(0.1), 5),
  b = prior_gamma(2, 5),
  beta = prior_normal(0, 2)
)
```

## Arguments

- a:

  (`Prior`) Prior distribution for the scale parameter `a`.

- b:

  (`Prior`) Prior distribution for the shape parameter `b`.

- beta:

  (`Prior`) Prior distribution for covariates coefficients `beta`.

## Value

A `SurvivalLogLogistic` object.

## Examples

``` r
SurvivalLogLogistic()
#> 
#> Log-Logistic Survival Model with parameters:
#>     sm_loglogis_a ~ lognormal(mu = -2.30259, sigma = 5)
#>     sm_loglogis_b ~ gamma(alpha = 2, beta = 5)
#>     beta_os_cov ~ normal(mu = 0, sigma = 2)
#> 
```
