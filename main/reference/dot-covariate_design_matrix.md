# Build a subject-level covariate design matrix

Builds a model matrix and removes its intercept column. Single-level
factors are supported and contribute no columns under reference coding.

## Usage

``` r
.covariate_design_matrix(formula, data, argument = "formula")
```

## Arguments

- formula:

  One-sided formula describing the subject-level covariates.

- data:

  Data frame containing one row per subject.

- argument:

  Name of the user-facing formula argument, used in errors.

## Value

A numeric design matrix without an intercept column.
