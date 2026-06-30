# Simulate Longitudinal Data from a GSF Model

Simulate Longitudinal Data from a GSF Model

## Usage

``` r
SimLongitudinalGSF(
  times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550)/365,
  sigma = 0.01,
  mu_s = log(c(0.6, 0.4)),
  mu_g = log(c(0.25, 0.35)),
  mu_b = log(60),
  mu_phi = qlogis(c(0.4, 0.6)),
  omega_b = 0.2,
  omega_s = 0.2,
  omega_g = 0.2,
  omega_phi = 0.2,
  link_dsld = 0,
  link_ttg = 0,
  link_identity = 0,
  link_growth = 0,
  link_shrinkage = 0,
  scaled_variance = FALSE
)
```

## Arguments

- times:

  (`numeric`) the times to generate observations at.

- sigma:

  (`number`) the variance of the longitudinal values.

- mu_s:

  (`numeric`) the mean shrinkage rates.

- mu_g:

  (`numeric`) the mean growth rates.

- mu_b:

  (`numeric`) the mean baseline values.

- mu_phi:

  (`numeric`) the mean proportion of cells affected by the treatment

- omega_b:

  (`number`) the baseline value standard deviation.

- omega_s:

  (`number`) the shrinkage rate standard deviation.

- omega_g:

  (`number`) the growth rate standard deviation.

- omega_phi:

  (`number`) for the standard deviation of the proportion of cells
  affected by the treatment `omega_phi`.

- link_dsld:

  (`number`) the link coefficient for the derivative contribution.

- link_ttg:

  (`number`) the link coefficient for the time-to-growth contribution.

- link_identity:

  (`number`) the link coefficient for the SLD Identity contribution.

- link_growth:

  (`number`) the link coefficient for the log-growth parameter
  contribution.

- link_shrinkage:

  (`number`) the link coefficient for the log-shrinkage parameter
  contribution.

- scaled_variance:

  (`logical`) whether the variance should be scaled by the expected
  value (see the "Statistical Specifications" vignette for more details)

## Slots

- `sigma`:

  (`numeric`)\
  See arguments.

- `mu_s`:

  (`numeric`)\
  See arguments.

- `mu_g`:

  (`numeric`)\
  See arguments.

- `mu_b`:

  (`numeric`)\
  See arguments.

- `mu_phi`:

  (`numeric`)\
  See arguments.

- `omega_b`:

  (`numeric`)\
  See arguments.

- `omega_s`:

  (`numeric`)\
  See arguments.

- `omega_g`:

  (`numeric`)\
  See arguments.

- `omega_phi`:

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

- `link_shrinkage`:

  (`numeric`)\
  See arguments.

- `scaled_variance`:

  (`logical`)\
  See arguments.

## See also

Other SimLongitudinal:
[`SimLongitudinal-class`](https://genentech.github.io/jmpost/reference/SimLongitudinal-class.md),
[`SimLongitudinalClaretBruno-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalClaretBruno-class.md),
[`SimLongitudinalRandomSlope-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalRandomSlope-class.md),
[`SimLongitudinalSteinFojo-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalSteinFojo-class.md)
