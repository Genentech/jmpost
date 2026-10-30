# Claret-Bruno longitudinal model with subject-level covariates

Separate covariate predictors model the log-normal distribution
parameters for baseline tumour size, growth, resistance, and growth
inhibition.

Formula intercept columns are removed because every predictor has a
separate intercept. Predictor parametrizations have the same meaning as
in
[`LongitudinalSteinFojoCov()`](https://genentech.github.io/jmpost/reference/LongitudinalSteinFojoCov-class.md).

## Usage

``` r
LongitudinalClaretBrunoCov(
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
  mu_b_intercept_prior = prior_normal(log(60), 1),
  mu_b_coefficients_prior = prior_normal(0, 1),
  omega_b_intercept_prior = prior_normal(log(0.2), 1),
  omega_b_coefficients_prior = prior_normal(0, 1),
  mu_g_intercept_prior = prior_normal(log(1), 1),
  mu_g_coefficients_prior = prior_normal(0, 1),
  omega_g_intercept_prior = prior_normal(log(0.2), 1),
  omega_g_coefficients_prior = prior_normal(0, 1),
  mu_c_intercept_prior = prior_normal(log(0.4), 1),
  mu_c_coefficients_prior = prior_normal(0, 1),
  omega_c_intercept_prior = prior_normal(log(0.2), 1),
  omega_c_coefficients_prior = prior_normal(0, 1),
  mu_p_intercept_prior = prior_normal(log(2), 1),
  mu_p_coefficients_prior = prior_normal(0, 1),
  omega_p_intercept_prior = prior_normal(log(0.2), 1),
  omega_p_coefficients_prior = prior_normal(0, 1),
  sigma = prior_lognormal(log(0.1), 1),
  scaled_variance = FALSE,
  centred_baseline = TRUE,
  centred_growth = FALSE,
  centred_resistance = FALSE,
  centred_inhibition = FALSE
)
```

## Arguments

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

- mu_b_intercept_prior:

  (`Prior`) Prior for the intercept of the baseline tumour size mean
  predictor.

- mu_b_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the baseline tumour
  size mean predictor.

- omega_b_intercept_prior:

  (`Prior`) Prior for the intercept of the baseline tumour size standard
  deviation predictor.

- omega_b_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the baseline tumour
  size standard deviation predictor.

- mu_g_intercept_prior:

  (`Prior`) Prior for the intercept of the growth mean predictor.

- mu_g_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the growth mean
  predictor.

- omega_g_intercept_prior:

  (`Prior`) Prior for the intercept of the growth standard deviation
  predictor.

- omega_g_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the growth standard
  deviation predictor.

- mu_c_intercept_prior:

  (`Prior`) Prior for the intercept of the resistance mean predictor.

- mu_c_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the resistance mean
  predictor.

- omega_c_intercept_prior:

  (`Prior`) Prior for the intercept of the resistance standard deviation
  predictor.

- omega_c_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the resistance
  standard deviation predictor.

- mu_p_intercept_prior:

  (`Prior`) Prior for the intercept of the growth inhibition mean
  predictor.

- mu_p_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the growth
  inhibition mean predictor.

- omega_p_intercept_prior:

  (`Prior`) Prior for the intercept of the growth inhibition standard
  deviation predictor.

- omega_p_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the growth
  inhibition standard deviation predictor.

- sigma:

  (`Prior`) Observation-error standard deviation prior.

- scaled_variance:

  (`logical`) Whether to use multiplicative observation error.

- centred_baseline:

  (`logical`) Whether to use centred parameterizations.

- centred_growth:

  (`logical`) Whether to use centred parameterizations.

- centred_resistance:

  (`logical`) Whether to use centred parameterizations.

- centred_inhibition:

  (`logical`) Whether to use centred parameterizations.

## Value

A `LongitudinalClaretBrunoCov` object.

## Available Links

- [`linkDSLD()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkTTG()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkIdentity()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkGrowth()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)
