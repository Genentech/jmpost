# Build covariate labels for sampled shrinkage parameters

Build covariate labels for sampled shrinkage parameters

## Usage

``` r
.shrinkage_covariate_names(object, parameters)
```

## Arguments

- object:

  A
  [`JointModelSamples`](https://genentech.github.io/jmpost/reference/JointModelSamples-class.md)
  object supplying model and data.

- parameters:

  Character vector of parameter names, without the
  `prior_shrinkage_factors_` prefix or element indices.

## Value

A named list of covariate labels for recognised parameters. Unmapped
parameters are omitted so the caller can retain their Stan names.
