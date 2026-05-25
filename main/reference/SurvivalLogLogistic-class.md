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
