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
