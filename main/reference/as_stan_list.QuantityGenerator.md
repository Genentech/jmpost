# `QuantityGenerator` -\> `list`

Converts a `QuantityGenerator` object to a list containing the required
input data for a stan model.

## Usage

``` r
# S3 method for class 'QuantityGenerator'
as_stan_list(object, data, ...)

# S3 method for class 'QuantityGeneratorPopulation'
as_stan_list(object, data, ...)

# S3 method for class 'QuantityGeneratorPrediction'
as_stan_list(object, data, model, ...)

# S3 method for class 'QuantityGeneratorSubject'
as_stan_list(object, data, ...)
```

## Arguments

- object:

  (`QuantityGenerator`)\
  object to convert to a list.

- data:

  (`DataJoint`)\
  Survival and Longitudinal Data.

- ...:

  Not currently used.
