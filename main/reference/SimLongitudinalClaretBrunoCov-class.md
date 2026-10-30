# Simulate a Claret-Bruno longitudinal model with covariates

Simulate a Claret-Bruno longitudinal model with covariates

## Usage

``` r
SimLongitudinalClaretBrunoCov(
  times = c(-100, -50, 0, 50, 100, 150, 250, 350, 450, 550)/365,
  mu_b_formula = ~study,
  omega_b_formula = ~study,
  mu_g_formula = ~arm,
  omega_g_formula = ~arm,
  mu_c_formula = ~arm,
  omega_c_formula = ~arm,
  mu_p_formula = ~arm,
  omega_p_formula = ~arm,
  mu_b_parametrization = "linear",
  omega_b_parametrization = "log-linear",
  mu_g_parametrization = "linear",
  omega_g_parametrization = "log-linear",
  mu_c_parametrization = "linear",
  omega_c_parametrization = "log-linear",
  mu_p_parametrization = "linear",
  omega_p_parametrization = "log-linear",
  mu_b_intercept = log(60),
  mu_b_coefficients = numeric(),
  omega_b_intercept = log(0.2),
  omega_b_coefficients = numeric(),
  mu_g_intercept = log(1),
  mu_g_coefficients = numeric(),
  omega_g_intercept = log(0.2),
  omega_g_coefficients = numeric(),
  mu_c_intercept = log(0.4),
  mu_c_coefficients = numeric(),
  omega_c_intercept = log(0.2),
  omega_c_coefficients = numeric(),
  mu_p_intercept = log(2),
  mu_p_coefficients = numeric(),
  omega_p_intercept = log(0.2),
  omega_p_coefficients = numeric(),
  sigma = 0.01,
  link_dsld = 0,
  link_ttg = 0,
  link_identity = 0,
  link_growth = 0,
  scaled_variance = FALSE
)
```

## Arguments

- times:

  (`numeric`) Observation times.

- mu_b_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the baseline tumour size mean.

- omega_b_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the baseline tumour size standard deviation.

- mu_g_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the growth mean.

- omega_g_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the growth standard deviation.

- mu_c_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the resistance mean.

- omega_c_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the resistance standard deviation.

- mu_p_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the growth inhibition mean.

- omega_p_formula:

  (`formula`) One-sided formula for subject-level covariate effects on
  the growth inhibition standard deviation.

- mu_b_parametrization:

  (`character`) Parametrization of the baseline tumour size mean
  predictor.

- omega_b_parametrization:

  (`character`) Parametrization of the baseline tumour size standard
  deviation predictor. Only `"exponential"` and `"log-linear"` are
  supported.

- mu_g_parametrization:

  (`character`) Parametrization of the growth mean predictor.

- omega_g_parametrization:

  (`character`) Parametrization of the growth standard deviation
  predictor. Only `"exponential"` and `"log-linear"` are supported.

- mu_c_parametrization:

  (`character`) Parametrization of the resistance mean predictor.

- omega_c_parametrization:

  (`character`) Parametrization of the resistance standard deviation
  predictor. Only `"exponential"` and `"log-linear"` are supported.

- mu_p_parametrization:

  (`character`) Parametrization of the growth inhibition mean predictor.

- omega_p_parametrization:

  (`character`) Parametrization of the growth inhibition standard
  deviation predictor. Only `"exponential"` and `"log-linear"` are
  supported.

- mu_b_intercept:

  (`numeric`) Intercept for the baseline tumour size mean predictor.

- mu_b_coefficients:

  (`numeric`) Covariate coefficients for the baseline tumour size mean
  predictor.

- omega_b_intercept:

  (`numeric`) Intercept for the baseline tumour size standard deviation
  predictor.

- omega_b_coefficients:

  (`numeric`) Covariate coefficients for the baseline tumour size
  standard deviation predictor.

- mu_g_intercept:

  (`numeric`) Intercept for the growth mean predictor.

- mu_g_coefficients:

  (`numeric`) Covariate coefficients for the growth mean predictor.

- omega_g_intercept:

  (`numeric`) Intercept for the growth standard deviation predictor.

- omega_g_coefficients:

  (`numeric`) Covariate coefficients for the growth standard deviation
  predictor.

- mu_c_intercept:

  (`numeric`) Intercept for the resistance mean predictor.

- mu_c_coefficients:

  (`numeric`) Covariate coefficients for the resistance mean predictor.

- omega_c_intercept:

  (`numeric`) Intercept for the resistance standard deviation predictor.

- omega_c_coefficients:

  (`numeric`) Covariate coefficients for the resistance standard
  deviation predictor.

- mu_p_intercept:

  (`numeric`) Intercept for the growth inhibition mean predictor.

- mu_p_coefficients:

  (`numeric`) Covariate coefficients for the growth inhibition mean
  predictor.

- omega_p_intercept:

  (`numeric`) Intercept for the growth inhibition standard deviation
  predictor.

- omega_p_coefficients:

  (`numeric`) Covariate coefficients for the growth inhibition standard
  deviation predictor.

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

- scaled_variance:

  (`logical`) Whether to use multiplicative observation error.

## Value

A `SimLongitudinalClaretBrunoCov` object.
