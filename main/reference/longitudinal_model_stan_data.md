# Create longitudinal-model-specific Stan data

Create longitudinal-model-specific Stan data

## Usage

``` r
longitudinal_model_stan_data(model, subject)

# Default S3 method
longitudinal_model_stan_data(model, subject)

# S3 method for class 'LongitudinalClaretBrunoCov'
longitudinal_model_stan_data(model, subject)

# S3 method for class 'LongitudinalGSFCov'
longitudinal_model_stan_data(model, subject)

# S3 method for class 'LongitudinalRandomSlopeCov'
longitudinal_model_stan_data(model, subject)

# S3 method for class 'LongitudinalSteinFojoCov'
longitudinal_model_stan_data(model, subject)
```

## Arguments

- model:

  A
  [`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  object.

- subject:

  A
  [`DataSubject`](https://genentech.github.io/jmpost/reference/DataSubject-class.md)
  object.

## Value

A named list of model-specific Stan data components.
