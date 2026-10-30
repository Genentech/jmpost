# Generalized Stein-Fojo longitudinal model with subject-level covariates

Separate covariate predictors model the log-normal distribution
parameters for baseline, shrinkage, growth, and the logit-normal
treatment fraction.

Formula intercept columns are removed because each predictor has a
separate intercept. `linear`, `proportional`, `exponential`, and
`log-linear` parametrizations have the same meaning as in
[`LongitudinalSteinFojoCov()`](https://genentech.github.io/jmpost/reference/LongitudinalSteinFojoCov-class.md).
The `mu_phi` predictor is the location of the normal distribution on the
logit scale; `phi` itself is obtained by applying the inverse-logit
transform.

## Usage

``` r
LongitudinalGSFCov(
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
  mu_b_intercept_prior = prior_normal(log(60), 1),
  mu_b_coefficients_prior = prior_normal(0, 1),
  omega_b_intercept_prior = prior_normal(log(0.2), 1),
  omega_b_coefficients_prior = prior_normal(0, 1),
  mu_s_intercept_prior = prior_normal(log(0.5), 1),
  mu_s_coefficients_prior = prior_normal(0, 1),
  omega_s_intercept_prior = prior_normal(log(0.2), 1),
  omega_s_coefficients_prior = prior_normal(0, 1),
  mu_g_intercept_prior = prior_normal(log(0.3), 1),
  mu_g_coefficients_prior = prior_normal(0, 1),
  omega_g_intercept_prior = prior_normal(log(0.2), 1),
  omega_g_coefficients_prior = prior_normal(0, 1),
  mu_phi_intercept_prior = prior_normal(qlogis(0.5), 1),
  mu_phi_coefficients_prior = prior_normal(0, 1),
  omega_phi_intercept_prior = prior_normal(log(0.2), 1),
  omega_phi_coefficients_prior = prior_normal(0, 1),
  sigma = prior_lognormal(log(0.1), 1),
  scaled_variance = FALSE,
  centred_baseline = TRUE,
  centred_shrinkage = FALSE,
  centred_growth = FALSE,
  centred_phi = FALSE
)
```

## Arguments

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

- mu_b_intercept_prior:

  (`Prior`) Prior for the intercept of the baseline mean predictor.

- mu_b_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the baseline mean
  predictor.

- omega_b_intercept_prior:

  (`Prior`) Prior for the intercept of the baseline standard deviation
  predictor.

- omega_b_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the baseline
  standard deviation predictor.

- mu_s_intercept_prior:

  (`Prior`) Prior for the intercept of the shrinkage mean predictor.

- mu_s_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the shrinkage mean
  predictor.

- omega_s_intercept_prior:

  (`Prior`) Prior for the intercept of the shrinkage standard deviation
  predictor.

- omega_s_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the shrinkage
  standard deviation predictor.

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

- mu_phi_intercept_prior:

  (`Prior`) Prior for the intercept of the mean predictor for the
  treatment fraction on the logit scale.

- mu_phi_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the mean predictor
  for the treatment fraction on the logit scale.

- omega_phi_intercept_prior:

  (`Prior`) Prior for the intercept of the standard deviation predictor
  for the treatment fraction on the logit scale.

- omega_phi_coefficients_prior:

  (`Prior`) Prior for the covariate coefficients of the standard
  deviation predictor for the treatment fraction on the logit scale.

- sigma:

  (`Prior`) Observation-error standard deviation prior.

- scaled_variance:

  (`logical`) Whether to use multiplicative observation error.

- centred_baseline:

  (`logical`) Whether to use centred parameterizations.

- centred_shrinkage:

  (`logical`) Whether to use centred parameterizations.

- centred_growth:

  (`logical`) Whether to use centred parameterizations.

- centred_phi:

  (`logical`) Whether to use centred parameterizations.

## Value

A `LongitudinalGSFCov` object.

## Available Links

- [`linkDSLD()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkTTG()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkIdentity()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkGrowth()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)

- [`linkShrinkage()`](https://genentech.github.io/jmpost/reference/standard-link-user.md)
