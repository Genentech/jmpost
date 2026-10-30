# Simulate a Generalized Stein-Fojo model with covariates

Simulate a Generalized Stein-Fojo model with covariates

## Usage

``` r
SimLongitudinalGSFCov(
  times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550)/365,
  mu_b_formula = ~study,
  omega_b_formula = ~study,
  mu_s_formula = ~arm,
  omega_s_formula = ~arm,
  mu_g_formula = ~arm,
  omega_g_formula = ~arm,
  mu_phi_formula = ~arm,
  omega_phi_formula = ~arm,
  mu_b_parametrization = "linear",
  omega_b_parametrization = "log-linear",
  mu_s_parametrization = "linear",
  omega_s_parametrization = "log-linear",
  mu_g_parametrization = "linear",
  omega_g_parametrization = "log-linear",
  mu_phi_parametrization = "linear",
  omega_phi_parametrization = "log-linear",
  mu_b_intercept = log(60),
  mu_b_coefficients = numeric(),
  omega_b_intercept = log(0.2),
  omega_b_coefficients = numeric(),
  mu_s_intercept = log(0.5),
  mu_s_coefficients = numeric(),
  omega_s_intercept = log(0.2),
  omega_s_coefficients = numeric(),
  mu_g_intercept = log(0.3),
  mu_g_coefficients = numeric(),
  omega_g_intercept = log(0.2),
  omega_g_coefficients = numeric(),
  mu_phi_intercept = qlogis(0.5),
  mu_phi_coefficients = numeric(),
  omega_phi_intercept = log(0.2),
  omega_phi_coefficients = numeric(),
  sigma = 0.01,
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

  (`numeric`) Observation times.

- mu_b_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the baseline mean.

- omega_b_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the baseline standard deviation.

- mu_s_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the shrinkage mean.

- omega_s_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the shrinkage standard deviation.

- mu_g_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the growth mean.

- omega_g_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the growth standard deviation.

- mu_phi_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the mean of the treatment fraction on the logit scale.

- omega_phi_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the standard deviation of the treatment fraction on the logit scale.

- mu_b_parametrization:

  (`character`) Parametrization of the baseline mean predictor.

- omega_b_parametrization:

  (`character`) Parametrization of the baseline standard deviation
  predictor. Only `"exponential"` and `"log-linear"` are supported.

- mu_s_parametrization:

  (`character`) Parametrization of the shrinkage mean predictor.

- omega_s_parametrization:

  (`character`) Parametrization of the shrinkage standard deviation
  predictor. Only `"exponential"` and `"log-linear"` are supported.

- mu_g_parametrization:

  (`character`) Parametrization of the growth mean predictor.

- omega_g_parametrization:

  (`character`) Parametrization of the growth standard deviation
  predictor. Only `"exponential"` and `"log-linear"` are supported.

- mu_phi_parametrization:

  (`character`) Parametrization of the mean predictor for the treatment
  fraction on the logit scale.

- omega_phi_parametrization:

  (`character`) Parametrization of the standard deviation predictor for
  the treatment fraction on the logit scale. Only `"exponential"` and
  `"log-linear"` are supported.

- mu_b_intercept:

  (`numeric`) Intercept for the baseline mean predictor.

- mu_b_coefficients:

  (`numeric`) Covariate coefficients for the baseline mean predictor.

- omega_b_intercept:

  (`numeric`) Intercept for the baseline standard deviation predictor.

- omega_b_coefficients:

  (`numeric`) Covariate coefficients for the baseline standard deviation
  predictor.

- mu_s_intercept:

  (`numeric`) Intercept for the shrinkage mean predictor.

- mu_s_coefficients:

  (`numeric`) Covariate coefficients for the shrinkage mean predictor.

- omega_s_intercept:

  (`numeric`) Intercept for the shrinkage standard deviation predictor.

- omega_s_coefficients:

  (`numeric`) Covariate coefficients for the shrinkage standard
  deviation predictor.

- mu_g_intercept:

  (`numeric`) Intercept for the growth mean predictor.

- mu_g_coefficients:

  (`numeric`) Covariate coefficients for the growth mean predictor.

- omega_g_intercept:

  (`numeric`) Intercept for the growth standard deviation predictor.

- omega_g_coefficients:

  (`numeric`) Covariate coefficients for the growth standard deviation
  predictor.

- mu_phi_intercept:

  (`numeric`) Intercept for the treatment fraction on the logit scale
  mean predictor.

- mu_phi_coefficients:

  (`numeric`) Covariate coefficients for the treatment fraction on the
  logit scale mean predictor.

- omega_phi_intercept:

  (`numeric`) Intercept for the treatment fraction on the logit scale
  standard deviation predictor.

- omega_phi_coefficients:

  (`numeric`) Covariate coefficients for the treatment fraction on the
  logit scale standard deviation predictor.

- sigma:

  (`numeric`) Observation-error standard deviation.

- link_dsld:

  (`numeric`) Coefficient for the DSLD association link.

- link_ttg:

  (`numeric`) Coefficient for the TTG association link.

- link_identity:

  (`numeric`) Coefficient for the identity association link.

- link_growth:

  (`numeric`) Coefficient for the growth association link.

- link_shrinkage:

  (`numeric`) Coefficient for the shrinkage association link.

- scaled_variance:

  (`logical`) Whether to use multiplicative observation error.

## Value

A `SimLongitudinalGSFCov` object.
