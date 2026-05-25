# `LinkComponent`

`LinkComponent`

## Usage

``` r
LinkComponent(stan, prior, key, ...)
```

## Arguments

- stan:

  (`StanModule`) Stan code. See Details.

- prior:

  (`Prior`) The prior for the scaling coeficient.

- key:

  (`character`) Link identifier. See Details.

- ...:

  additional arguments for
  [`StanModel()`](https://genentech.github.io/jmpost/reference/StanModel-class.md).

## Details

This object provides key information needed to construct a link
contribution in the survival model based on the parameters of the
longitudinal model.

Each link component defines a stan function of the longitudinal model
parameters which is multiplied by a model coefficient and added to the
survival models hazard function.

For full details about the specification of a `LinkComponent` please see
[`vignette("extending-jmpost", package = "jmpost")`](https://genentech.github.io/jmpost/articles/extending-jmpost.md).

## Slots

- `stan`:

  (`StanModule`)\
  See Arguments.

- `name`:

  (`character`)\
  See Arguments.

- `parameters`:

  (`ParameterList`)\
  The parameter specification.

## See also

Other LinkComponent:
[`as.StanModule.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.StanModule.LinkComponent.md),
[`as.list.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.list.LinkComponent.md),
[`getParameters()`](https://genentech.github.io/jmpost/reference/getParameters.md),
[`initialValues()`](https://genentech.github.io/jmpost/reference/initialValues.md)
