# `auxiliarySize`

Obtain sizes for any additional Stan parameters introduced by an object.

## Usage

``` r
auxiliarySize(object, ...)
```

## Arguments

- object:

  where to get the auxiliary parameter sizes from.

- ...:

  additional options.

## Value

A named `list` of auxiliary parameter sizes.

## Details

Some objects, such as shrinkage priors, declare additional Stan
parameters beyond the main model parameter. This helper returns named
sizes for those additional parameters so their initial values can be
expanded consistently.
