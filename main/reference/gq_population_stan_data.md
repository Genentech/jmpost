# Population Generated-Quantities Stan Data

Obtain the model-specific Stan declarations and data needed for
longitudinal population generated quantities.

## Usage

``` r
gq_population_stan_data(object, model, data = NULL, ...)

# Default S3 method
gq_population_stan_data(object, model, data = NULL, ...)

# S3 method for class 'LongitudinalClaretBrunoCov'
gq_population_stan_data(object, model, data = NULL, ...)

# S3 method for class 'LongitudinalGSFCov'
gq_population_stan_data(object, model, data = NULL, ...)

# S3 method for class 'LongitudinalRandomSlopeCov'
gq_population_stan_data(object, model, data = NULL, ...)

# S3 method for class 'LongitudinalSteinFojoCov'
gq_population_stan_data(object, model, data = NULL, ...)

gq_population_stan_data.QuantityGeneratorPopulation(
  object,
  model,
  data = NULL,
  ...
)
```

## Arguments

- object:

  A quantity generator.

- model:

  A longitudinal model.

- data:

  A
  [`DataJoint`](https://genentech.github.io/jmpost/reference/DataJoint-class.md)
  object, or `NULL` when only declarations are required.

- ...:

  Additional options.

## Value

A list with `declarations`, a character scalar of Stan data-block
declarations, and `data`, a named list of Stan data values.

## Details

This generic first dispatches on the quantity generator. Its
[`QuantityGeneratorPopulation`](https://genentech.github.io/jmpost/reference/Quant-Dev.md)
method then dispatches on `model`, allowing longitudinal-model authors
to provide a `gq_population_stan_data.<model-class>()` method alongside
their model.
