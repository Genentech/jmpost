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

  (`Prior`) A
  [`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md)
  object.

- model:

  (`LongitudinalModel`) A
  [`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  object.

- ...:

  Not used.

## Value

A `Link` or `LinkComponent` object representing the requested
association.

## Functions

- `linkNone()`: No link (fit the survival and longitudinal models
  independently)

- `linkTTG()`: Time to growth link

- `linkDSLD()`: Derivative of the SLD over time link

- `linkIdentity()`: Current SLD value link

- `linkGrowth()`: Growth Parameter link

- `linkShrinkage()`: Shrinkage Parameter link

## Examples

``` r
linkNone()
#> 
#> No Link
linkDSLD()
#> 
#> LinkComponent with parameter:
#>     link_dsld ~ normal(mu = 0, sigma = 2)
#> 
linkTTG(prior_normal(0, 1), model = LongitudinalGSF())
#> 
#> LinkComponent with parameter:
#>     link_ttg ~ normal(mu = 0, sigma = 1)
#> 
```
