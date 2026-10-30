# Summarise posterior covariate coefficients

Extracts posterior summaries of survival and longitudinal covariate
coefficients from a fitted joint model. Uses the existing
[`stats::coef()`](https://rdrr.io/r/stats/coef.html) generic.
Survival-only and longitudinal-only fits are also supported.

## Usage

``` r
# S3 method for class 'JointModelSamples'
coef(object, ...)
```

## Arguments

- object:

  (`JointModelSamples`) samples from a fitted joint model.

- ...:

  Summary functions or named arguments passed to the underlying
  [`cmdstanr::CmdStanMCMC`](https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html)
  object's `$summary()` method, which uses
  [`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html).
  Supply a custom quantile function to change the credible interval (see
  examples). Do not supply `variables`: the method selects the
  coefficient variables automatically.

## Value

A tibble with one row per covariate coefficient, in design-matrix order
(survival first, then longitudinal predictors). The columns are:

- `component`: `"survival"` or `"longitudinal"`.

- `predictor`: the longitudinal predictor name, or `NA` for survival.

- `covariate`: the corresponding design-matrix column name.

- `variable`: the original indexed Stan variable name.

- Summary columns returned by `$summary()`. By default these include
  `mean`, `median`, `sd`, `mad`, `q5`, `q95`, `rhat`, `ess_bulk`, and
  `ess_tail`. The default intervals are 90% credible intervals.

## Details

Covariate labels come from the fitted design matrices, including factor
contrasts and interactions. Longitudinal coefficients are grouped by
their predictor, such as `mu_g`, `mu_s`, or `omega_g`; all covariate
predictors reported by
[`covariates.LongitudinalModel()`](https://genentech.github.io/jmpost/reference/covariates.LongitudinalModel.md)
are included.

Coefficients remain on their fitted predictor scales. No exponentiation
or other transformation is applied. Intercepts, association-link
coefficients, and other model parameters are not included. An error is
raised if there are no covariate coefficients or a required coefficient
was not sampled.

## See also

[`shrinkage.JointModelSamples()`](https://genentech.github.io/jmpost/reference/shrinkage.JointModelSamples.md)

Other JointModelSamples:
[`LongitudinalRandomEffects()`](https://genentech.github.io/jmpost/reference/LongitudinalRandomEffects.md),
[`as_print_string.JointModelSamples()`](https://genentech.github.io/jmpost/reference/as_print_string.JointModelSamples.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# fit is a JointModelSamples object returned by sampleStanModel().
coef(fit)
coef(fit, "median", intervals = function(x) {
    stats::quantile(x, probs = c(0.025, 0.975))
})
coef(fit, "mean", "sd")
} # }
```
