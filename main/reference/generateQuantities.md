# `generateQuantities`

Obtain the generated quantities from a Stan Model.

## Usage

``` r
generateQuantities(object, ...)

# S3 method for class 'JointModelSamples'
generateQuantities(object, generator, type, ...)
```

## Arguments

- object:

  object to obtain generated quantities from

- ...:

  additional options.

- generator:

  (`QuantityGenerator`)\
  object that specifies which subjects and time points to calculate the
  quantities at

- type:

  (`character`)\
  type of quantities to be generated, must be either "survival" or
  "longitudinal".
