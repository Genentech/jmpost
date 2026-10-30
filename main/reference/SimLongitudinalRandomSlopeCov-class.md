# Simulate a random-slope longitudinal model with covariates

Simulate a random-slope longitudinal model with covariates

Construct a random-slope simulator with subject-level covariates

## Usage

``` r
SimLongitudinalRandomSlopeCov(
  times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550),
  mu_formula = ~study,
  slope_mu_formula = ~arm,
  slope_sigma_formula = ~arm,
  mu_parametrization = "linear",
  slope_mu_parametrization = "linear",
  slope_sigma_parametrization = "log-linear",
  mu_intercept = 50,
  mu_coefficients = numeric(),
  slope_mu_intercept = 0.01,
  slope_mu_coefficients = numeric(),
  slope_sigma_intercept = log(0.5),
  slope_sigma_coefficients = numeric(),
  sigma = 2,
  link_dsld = 0,
  link_identity = 0,
  scaled_variance = FALSE
)
```

## Arguments

- times:

  (`numeric`) Observation times.

- mu_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the subject intercept.

- slope_mu_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the mean slope.

- slope_sigma_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the slope standard deviation.

- mu_parametrization:

  (`character`) Parametrization of the subject intercept predictor.

- slope_mu_parametrization:

  (`character`) Parametrization of the mean slope predictor.

- slope_sigma_parametrization:

  (`character`) Parametrization of the slope standard deviation
  predictor. Only `"exponential"` and `"log-linear"` are supported.

- mu_intercept:

  (`numeric`) Intercept for the subject intercept predictor.

- mu_coefficients:

  (`numeric`) Covariate coefficients for the subject intercept
  predictor.

- slope_mu_intercept:

  (`numeric`) Intercept for the mean slope predictor.

- slope_mu_coefficients:

  (`numeric`) Covariate coefficients for the mean slope predictor.

- slope_sigma_intercept:

  (`numeric`) Intercept for the slope standard deviation predictor.

- slope_sigma_coefficients:

  (`numeric`) Covariate coefficients for the slope standard deviation
  predictor.

- sigma:

  (`numeric`) Observation-error standard deviation.

- link_dsld:

  (`numeric`) Coefficient for the DSLD association link.

- link_identity:

  (`numeric`) Coefficient for the identity association link.

- scaled_variance:

  (`logical`) Whether to use multiplicative observation error.

## Value

A `SimLongitudinalRandomSlopeCov` object.
