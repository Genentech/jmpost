# Apply positivity constraints to a predictor intercept prior

Apply positivity constraints to a predictor intercept prior

## Usage

``` r
.positive_intercept_prior(prior, parametrization, positive = FALSE)
```

## Arguments

- prior:

  A
  [`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md)
  object for the predictor intercept.

- parametrization:

  Character scalar naming the predictor parametrization.

- positive:

  Whether the resulting parameter must be positive even when the
  parametrization itself does not require a positive intercept.

## Value

The input prior, with a positive lower limit when required.
