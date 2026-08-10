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

  (`QuantityGenerator`) object that specifies which subjects and time
  points to calculate the quantities at

- type:

  (`character`) type of quantities to be generated, must be either
  "survival" or "longitudinal".

## Value

A
[`cmdstanr::CmdStanGQ`](https://mc-stan.org/cmdstanr/reference/CmdStanGQ.html)
object containing generated quantities.

## Examples

``` r
if (FALSE) { # \dontrun{
generateQuantities(fit, generator, "survival")
} # }
```
