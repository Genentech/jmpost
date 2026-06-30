# `LongitudinalClaretBruno`

This class extends the general
[`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
class for using the Claret-Bruno model for the longitudinal outcome.

## Usage

``` r
LongitudinalClaretBruno(
  mu_b = prior_normal(log(60), 0.5),
  mu_g = prior_normal(log(1), 0.5),
  mu_c = prior_normal(log(0.4), 0.5),
  mu_p = prior_normal(log(2), 0.5),
  omega_b = prior_lognormal(log(0.2), 0.5),
  omega_g = prior_lognormal(log(0.2), 0.5),
  omega_c = prior_lognormal(log(0.2), 0.5),
  omega_p = prior_lognormal(log(0.2), 0.5),
  sigma = prior_lognormal(log(0.1), 0.5),
  scaled_variance = FALSE,
  centred = FALSE
)
```

## Arguments

- mu_b:

  (`Prior`) for the mean population baseline sld value.

- mu_g:

  (`Prior`) for the mean population growth rate.

- mu_c:

  (`Prior`) for the mean population resistance rate.

- mu_p:

  (`Prior`) for the mean population growth inhibition

- omega_b:

  (`Prior`) for the population standard deviation for the baseline sld
  value.

- omega_g:

  (`Prior`) for the population standard deviation for the growth rate.

- omega_c:

  (`Prior`) for the population standard deviation for the resistance
  rate.

- omega_p:

  (`Prior`) for the population standard deviation for the growth
  inhibition.

- sigma:

  (`Prior`) for the variance of the longitudinal values.

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
