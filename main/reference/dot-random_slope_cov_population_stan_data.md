# Create population-prediction Stan data for a covariate random-slope model

Create population-prediction Stan data for a covariate random-slope
model

## Usage

``` r
.random_slope_cov_population_stan_data(model, subject, newdata)
```

## Arguments

- model:

  A
  [`LongitudinalRandomSlopeCov`](https://genentech.github.io/jmpost/reference/LongitudinalRandomSlopeCov-class.md)
  object.

- subject:

  A
  [`DataSubject`](https://genentech.github.io/jmpost/reference/DataSubject-class.md)
  object containing the fitted subject data.

- newdata:

  Population profiles supplied through
  [`GridPopulation()`](https://genentech.github.io/jmpost/reference/Grid-Functions.md).

## Value

A named list of population generated-quantities design matrices.
