# `LongitudinalModel`

This class extends the general
[`StanModel`](https://genentech.github.io/jmpost/reference/StanModel-class.md)
class to comprise the longitudinal model specification.

## Usage

``` r
LongitudinalModel(
  stan = StanModule(),
  parameters = ParameterList(),
  name = "<Unnamed>",
  ...
)
```

## Arguments

- stan:

  (`StanModule`) code containing the Stan code specification.

- parameters:

  (`ParameterList`) the parameter specification.

- name:

  (`character`) display name for the model object.

- ...:

  additional arguments for
  [`StanModel()`](https://genentech.github.io/jmpost/reference/StanModel-class.md).
