# Build a covariate design matrix for new subject profiles

Uses the fitted subject data to preserve factor levels, contrasts, and
column order when constructing a design matrix for population
predictions.

## Usage

``` r
.covariate_prediction_design_matrix(
  formula,
  newdata,
  reference_data,
  argument = "formula"
)
```

## Arguments

- formula:

  One-sided formula describing the subject-level covariates.

- newdata:

  Data frame containing the population profiles to predict.

- reference_data:

  Subject data used to fit the model.

- argument:

  Name of the user-facing formula argument, used in errors.

## Value

A numeric design matrix matching the fitted design matrix.
