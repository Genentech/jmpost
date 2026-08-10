# `SurvivalExponential`

This class extends the general
[`SurvivalModel`](https://genentech.github.io/jmpost/reference/SurvivalModel-class.md)
class for using the exponential survival model.

## Usage

``` r
SurvivalExponential(lambda = prior_gamma(2, 5), beta = prior_normal(0, 2))
```

## Arguments

- lambda:

  (`Prior`) for the exponential rate `lambda`.

- beta:

  (`Prior`) for covariates coefficients `beta`.

## Value

A `SurvivalExponential` object.

## Examples

``` r
SurvivalExponential()
#> 
#> Exponential Survival Model with parameters:
#>     sm_exp_lambda ~ gamma(alpha = 2, beta = 5)
#>     beta_os_cov ~ normal(mu = 0, sigma = 2)
#> 
```
