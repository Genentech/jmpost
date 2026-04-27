# summary

This method returns a summary statistic `data.frame` of the random
effect parameters

## Usage

``` r
# S3 method for class 'RandomEffectQuantities'
summary(object, conf.level = 0.95, ...)
```

## Arguments

- object:

  ([`RandomEffectQuantities`](https://genentech.github.io/jmpost/reference/RandomEffectQuantities-class.md))\
  generated quantities.

- conf.level:

  (`numeric`)\
  confidence level of the interval.

- ...:

  not used.

## Value

A `data.frame` with the following variables:

- `subject` (`character`)\
  the subject identifier.

- `parameter` (`character`)\
  the parameter identifier.

- `median` (`numeric`)\
  the median value of the quantity.

- `lower` (`numeric`)\
  the lower CI value of the quantity.

- `upper` (`numeric`)\
  the upper CI value of the quantity.

## See also

Other RandomEffectQuantities:
[`LongitudinalRandomEffects()`](https://genentech.github.io/jmpost/reference/LongitudinalRandomEffects.md),
[`RandomEffectQuantities-class`](https://genentech.github.io/jmpost/reference/RandomEffectQuantities-class.md),
[`as.data.frame.RandomEffectQuantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.RandomEffectQuantities.md),
[`as_print_string.RandomEffectQuantities()`](https://genentech.github.io/jmpost/reference/as_print_string.RandomEffectQuantities.md)
