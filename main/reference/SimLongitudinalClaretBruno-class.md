# Simulate Longitudinal Data from a Claret-Bruno Model

Simulate Longitudinal Data from a Claret-Bruno Model

## Usage

``` r
SimLongitudinalClaretBruno(
  times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550)/365,
  sigma = 0.01,
  mu_b = log(60),
  mu_g = log(c(0.9, 1.1)),
  mu_c = log(c(0.25, 0.35)),
  mu_p = log(c(1.5, 2)),
  omega_b = 0.2,
  omega_g = 0.2,
  omega_c = 0.2,
  omega_p = 0.2,
  link_dsld = 0,
  link_ttg = 0,
  link_identity = 0,
  link_growth = 0,
  scaled_variance = TRUE
)
```

## Arguments

- times:

  (`numeric`)\
  the times to generate observations at.

- sigma:

  (`number`)\
  the variance of the longitudinal values.

- mu_b:

  (`numeric`)\
  the mean population baseline sld value.

- mu_g:

  (`numeric`)\
  the mean population growth rate.

- mu_c:

  (`numeric`)\
  the mean population resistance rate.

- mu_p:

  (`numeric`)\
  the mean population growth inhibition.

- omega_b:

  (`number`)\
  the population standard deviation for the baseline sld value.

- omega_g:

  (`number`)\
  the population standard deviation for the growth rate.

- omega_c:

  (`number`)\
  the population standard deviation for the resistance rate.

- omega_p:

  (`number`)\
  the population standard deviation for the growth inhibition.

- link_dsld:

  (`number`)\
  the link coefficient for the derivative contribution.

- link_ttg:

  (`number`)\
  the link coefficient for the time-to-growth contribution.

- link_identity:

  (`number`)\
  the link coefficient for the SLD Identity contribution.

- link_growth:

  (`number`)\
  the link coefficient for the growth parameter contribution.

- scaled_variance:

  (`logical`)\
  whether the variance should be scaled by the expected value (see the
  "Statistical Specifications" vignette for more details)

## Slots

- `sigma`:

  (`numeric`)\
  See arguments.

- `mu_b`:

  (`numeric`)\
  See arguments.

- `mu_g`:

  (`numeric`)\
  See arguments.

- `mu_c`:

  (`numeric`)\
  See arguments.

- `mu_p`:

  (`numeric`)\
  See arguments.

- `omega_b`:

  (`numeric`)\
  See arguments.

- `omega_g`:

  (`numeric`)\
  See arguments.

- `omega_c`:

  (`numeric`)\
  See arguments.

- `omega_p`:

  (`numeric`)\
  See arguments.

- `link_dsld`:

  (`numeric`)\
  See arguments.

- `link_ttg`:

  (`numeric`)\
  See arguments.

- `link_identity`:

  (`numeric`)\
  See arguments.

- `link_growth`:

  (`numeric`)\
  See arguments.

- `scaled_variance`:

  (`logical`)\
  See arguments.

## See also

Other SimLongitudinal:
[`SimLongitudinal-class`](https://genentech.github.io/jmpost/reference/SimLongitudinal-class.md),
[`SimLongitudinalGSF-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalGSF-class.md),
[`SimLongitudinalRandomSlope-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalRandomSlope-class.md),
[`SimLongitudinalSteinFojo-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalSteinFojo-class.md)
