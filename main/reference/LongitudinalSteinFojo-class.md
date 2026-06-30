# `LongitudinalSteinFojo`

This class extends the general
[`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
class for using the Stein-Fojo model for the longitudinal outcome.

## Usage

``` r
LongitudinalSteinFojo(
  mu_bsld = prior_normal(log(60), 1),
  mu_ks = prior_normal(log(0.5), 1),
  mu_kg = prior_normal(log(0.3), 1),
  omega_bsld = prior_lognormal(log(0.2), 1),
  omega_ks = prior_lognormal(log(0.2), 1),
  omega_kg = prior_lognormal(log(0.2), 1),
  sigma = prior_lognormal(log(0.1), 1),
  scaled_variance = FALSE,
  centred = FALSE
)
```

## Arguments

- mu_bsld:

  (`Prior`) for the mean baseline value `mu_bsld`.

- mu_ks:

  (`Prior`) for the mean shrinkage rate `mu_ks`.

- mu_kg:

  (`Prior`) for the mean growth rate `mu_kg`.

- omega_bsld:

  (`Prior`) for the baseline value standard deviation `omega_bsld`.

- omega_ks:

  (`Prior`) for the shrinkage rate standard deviation `omega_ks`.

- omega_kg:

  (`Prior`) for the growth rate standard deviation `omega_kg`.

- sigma:

  (`Prior`) for the variance of the longitudinal values `sigma`.

- scaled_variance:

  (`logical`) whether the variance should be scaled by the expected
  value, corresponding to a multiplicative model. As a default, the
  variance is not scaled by the expected value, corresponding to an
  additive model. (See the "Statistical Specifications" vignette for
  more details.)

- centred:

  (`logical`) whether to use the centred parameterization.

## Available Links

- [`linkDSLD()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkTTG()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkIdentity()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkGrowth()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)
