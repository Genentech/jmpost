# Standard Links

These functions are used to enable the use of the corresponding link
function between the survival and longitudinal models in a joint model.
Note that the exact implementation of the link function is model
specific, see
`vignette("Statistical Specifications", package = "jmpost")` for more
details.

## Usage

``` r
linkNone()

linkTTG(prior, model = PromiseLongitudinalModel(), ...)

linkDSLD(prior, model = PromiseLongitudinalModel(), ...)

linkIdentity(prior, model = PromiseLongitudinalModel(), ...)

linkGrowth(prior, model = PromiseLongitudinalModel(), ...)

linkShrinkage(prior, model = PromiseLongitudinalModel(), ...)
```

## Arguments

- prior:

  ([`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md))\
  A
  [`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md)
  object.

- model:

  ([`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md))\
  A
  [`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  object.

- ...:

  Not used.

## Functions

- `linkNone()`: No link (fit the survival and longitudinal models
  independently)

- `linkTTG()`: Time to growth link

- `linkDSLD()`: Derivative of the SLD over time link

- `linkIdentity()`: Current SLD value link

- `linkGrowth()`: Growth Parameter link

- `linkShrinkage()`: Shrinkage Parameter link
