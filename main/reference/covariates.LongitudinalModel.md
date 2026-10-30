# Extract covariate names for longitudinal model parameters

Uses the subject-level design matrices prepared for fitting the model.
Covariate names therefore include factor contrasts, transformations, and
interactions, in the same order as the corresponding coefficients.
Covariates are taken from subject data, rather than repeated
measurements in a
[`DataLongitudinal`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md)
object.

## Usage

``` r
# S3 method for class 'LongitudinalModel'
covariates(object, data, ...)
```

## Arguments

- object:

  (`LongitudinalModel`) the longitudinal model specification.

- data:

  (`DataSubject`) the subject data used to construct the model's design
  matrices.

- ...:

  not used.

## Value

A named list of character vectors, keyed by coefficient parameter name.
Intercept-only predictors have an empty character vector. Models without
covariate predictors, such as
[`LongitudinalSteinFojo`](https://genentech.github.io/jmpost/reference/LongitudinalSteinFojo-class.md),
return an empty list.

## Details

The default longitudinal-model method recognises design matrices named
`<name>_design` paired with declared parameters named
`<name>_coefficients`. Custom longitudinal models using another
convention can provide their own
[`covariates()`](https://genentech.github.io/jmpost/reference/covariates.md)
method with the same return structure.

## See also

Other covariates:
[`covariates()`](https://genentech.github.io/jmpost/reference/covariates.md),
[`covariates.DataSurvival()`](https://genentech.github.io/jmpost/reference/covariates.DataSurvival.md)

## Examples

``` r
subject <- DataSubject(
    data.frame(
        id = c("1", "2"),
        arm = c("A", "B"),
        study = "Study 1",
        age = c(50, 60)
    ),
    "id", "arm", "study"
)
model <- LongitudinalSteinFojoCov(mu_g_formula = ~ arm + age)
covariates(model, data = subject)$lm_sfc_mu_g_coefficients
#> [1] "armB" "age" 
covariates(LongitudinalSteinFojo(), data = subject)
#> list()
```
