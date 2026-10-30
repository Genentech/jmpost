# Render a covariate predictor as Stan code

Render a covariate predictor as Stan code

## Usage

``` r
.covariate_predictor_stan(
  prefix,
  parametrization,
  design_prefix = prefix,
  n_rows = "n_subjects"
)
```

## Arguments

- prefix:

  Prefix shared by the intercept and coefficient Stan variables.

- parametrization:

  Character scalar naming the predictor parametrization.

- design_prefix:

  Prefix for the design-matrix Stan variable.

- n_rows:

  Stan expression giving the number of predictor rows.

## Value

A character scalar containing a Stan expression.
