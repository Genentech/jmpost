# `SurvivalGamma`

This class extends the general
[`SurvivalModel`](https://genentech.github.io/jmpost/reference/SurvivalModel-class.md)
class for using the Gamma survival model.

## Usage

``` r
SurvivalGamma(
  k = prior_gamma(2, 0.5),
  theta = prior_gamma(2, 0.5),
  beta = prior_normal(0, 2)
)
```

## Arguments

- k:

  (`Prior`) for the shape `k`.

- theta:

  (`Prior`) for the scale `theta`.

- beta:

  (`Prior`) for covariates coefficients `beta`.

## Value

A `SurvivalGamma` object.

## Examples

``` r
SurvivalGamma()
#> 
#> Gamma Survival Model with parameters:
#>     sm_gamma_k ~ gamma(alpha = 2, beta = 0.5)
#>     sm_gamma_theta ~ gamma(alpha = 2, beta = 0.5)
#>     beta_os_cov ~ normal(mu = 0, sigma = 2)
#> 
```
