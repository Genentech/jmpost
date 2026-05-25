# summary

This method returns a summary statistic `data.frame` of the quantities.
Note that this is just an internal utility method in order to share
common code between
[LongitudinalQuantities](https://genentech.github.io/jmpost/reference/LongitudinalQuantities-class.md)
and
[SurvivalQuantities](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md)

## Usage

``` r
# S3 method for class 'Quantities'
summary(object, conf.level = 0.95, ...)
```

## Arguments

- object:

  (`Quantities`) generated quantities.

- conf.level:

  (`numeric`) confidence level of the interval.

- ...:

  not used.

## Value

A `data.frame` with the following variables:

- `median` (`numeric`)\
  the median value of the quantity.

- `lower` (`numeric`)\
  the lower CI value of the quantity.

- `upper` (`numeric`)\
  the upper CI value of the quantity.

- `time` (`numeric`)\
  the time point which the quantity is for.

- `group` (`character`)\
  which group the quantity belongs to.

- `type` (`character`)\
  what type of quantity is it.

## See also

Other Quantities:
[`Quantities-class`](https://genentech.github.io/jmpost/reference/Quantities-class.md),
[`as.data.frame.Quantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.Quantities.md),
[`as_print_string.Quantities()`](https://genentech.github.io/jmpost/reference/as_print_string.Quantities.md)
