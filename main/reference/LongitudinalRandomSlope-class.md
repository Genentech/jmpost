# `LongitudinalRandomSlope`

This class extends the general
[`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
class for using the random slope linear model for the longitudinal
outcome.

## Usage

``` r
LongitudinalRandomSlope(
  intercept = prior_normal(30, 10),
  slope_mu = prior_normal(1, 3),
  slope_sigma = prior_lognormal(0, 1.5),
  sigma = prior_lognormal(0, 1.5),
  scaled_variance = FALSE
)
```

## Arguments

- intercept:

  (`Prior`) for the `intercept`.

- slope_mu:

  (`Prior`) for the population slope `slope_mu` (one per arm).

- slope_sigma:

  (`Prior`) for the random slope standard deviation `slope_sigma` (one
  per arm).

- sigma:

  (`Prior`) for the variance of the longitudinal values `sigma`.

- scaled_variance:

  (`logical`) whether the variance should be scaled by the expected
  value, corresponding to a multiplicative model. As a default, the
  variance is not scaled by the expected value, corresponding to an
  additive model. (See the "Statistical Specifications" vignette for
  more details.)

## Available Links

- [`linkDSLD()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkIdentity()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)
