# Validate a covariate-predictor parametrization

Validate a covariate-predictor parametrization

## Usage

``` r
.validate_covariate_parametrization(x, argument, scale = FALSE)
```

## Arguments

- x:

  Character scalar naming the parametrization.

- argument:

  Name of the user-facing argument, used in error messages.

- scale:

  Whether the predictor represents a standard deviation.

## Value

`x`, validated against the supported parametrizations.
