# Simulate Longitudinal Data from a Random Slope Model

Simulate Longitudinal Data from a Random Slope Model

## Usage

``` r
SimLongitudinalRandomSlope(
  times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550),
  intercept = 50,
  slope_mu = c(0.01, 0.03),
  slope_sigma = 0.5,
  sigma = 2,
  link_dsld = 0,
  link_identity = 0
)
```

## Arguments

- times:

  (`numeric`)\
  the times to generate observations at.

- intercept:

  (`number`)\
  the mean baseline value for each study.

- slope_mu:

  (`numeric`)\
  the population slope for each treatment arm.

- slope_sigma:

  (`number`)\
  the random slope standard deviation.

- sigma:

  (`number`)\
  the variance of the longitudinal values.

- link_dsld:

  (`number`)\
  the link coefficient for the DSLD contribution.

- link_identity:

  (`number`)\
  the link coefficient for the identity contribution.

## Slots

- `intercept`:

  (`numeric`)\
  See arguments.

- `slope_mu`:

  (`numeric`)\
  See arguments.

- `slope_sigma`:

  (`numeric`)\
  See arguments.

- `sigma`:

  (`numeric`)\
  See arguments.

- `link_dsld`:

  (`numeric`)\
  See arguments.

- `link_identity`:

  (`numeric`)\
  See arguments.

## See also

Other SimLongitudinal:
[`SimLongitudinal-class`](https://genentech.github.io/jmpost/reference/SimLongitudinal-class.md),
[`SimLongitudinalClaretBruno-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalClaretBruno-class.md),
[`SimLongitudinalGSF-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalGSF-class.md),
[`SimLongitudinalSteinFojo-class`](https://genentech.github.io/jmpost/reference/SimLongitudinalSteinFojo-class.md)
