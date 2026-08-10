# `auxiliaryInitialValues`

Obtain initial values for any additional Stan parameters introduced by
an object.

## Usage

``` r
auxiliaryInitialValues(object, ...)
```

## Arguments

- object:

  where to get the auxiliary initial values from.

- ...:

  additional options.

## Value

A named `list` of initial values for auxiliary parameters.

## Details

Some objects, such as shrinkage priors, declare additional Stan
parameters beyond the main model parameter. This helper returns named
initial values for those additional parameters.
