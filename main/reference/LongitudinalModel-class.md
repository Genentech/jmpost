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
  scaled_variance = NA,
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

- scaled_variance:

  (`flag`) whether the variance should be scaled by the expected value,
  corresponding to a multiplicative model. If not, then an additive
  error model is used.

- ...:

  additional arguments for
  [`StanModel()`](https://genentech.github.io/jmpost/reference/StanModel-class.md).

## Slots

- `scaled_variance`:

  logical scalar, whether the variance should be scaled by the expected
  value, corresponding to a multiplicative model. If not, then an
  additive error model is used.
