# Enable Generated Quantities Generic

Enable Generated Quantities Generic

## Usage

``` r
enableGQ(object, ...)
```

## Arguments

- object:

  (`StanModel`) to enable generated quantities for.

- ...:

  Not used.

  Optional hook method that is called on a
  [`StanModel`](https://genentech.github.io/jmpost/reference/StanModel-class.md)
  if attempting to use either
  [`LongitudinalQuantities`](https://genentech.github.io/jmpost/reference/LongitudinalQuantities-class.md)
  or
  [`SurvivalQuantities`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md)

## Value

[`StanModule`](https://genentech.github.io/jmpost/reference/StanModule-class.md)
object
