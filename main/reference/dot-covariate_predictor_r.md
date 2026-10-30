# Evaluate a covariate predictor in R

Evaluate a covariate predictor in R

## Usage

``` r
.covariate_predictor_r(design, intercept, coefficients, parametrization)
```

## Arguments

- design:

  Numeric subject-level design matrix.

- intercept:

  Numeric scalar predictor intercept.

- coefficients:

  Numeric vector with one coefficient per design column.

- parametrization:

  Character scalar naming the predictor parametrization.

## Value

A numeric vector with one predicted value per design row.
