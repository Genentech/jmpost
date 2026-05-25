# `JointModelSamples` -\> `StanModule`

Converts a `JointModelSamples` object into a `StanModule` object
ensuring that the resulting `StanModule` object is able to generate post
sampling quantities.

## Usage

``` r
# S3 method for class 'JointModelSamples'
as.StanModule(object, generator, type, ...)
```

## Arguments

- object:

  object to obtain generated quantities from

- generator:

  (`QuantityGenerator`) object that specifies which subjects and time
  points to calculate the quantities at

- type:

  (`character`) type of quantities to be generated, must be either
  "survival" or "longitudinal".

- ...:

  additional options.
