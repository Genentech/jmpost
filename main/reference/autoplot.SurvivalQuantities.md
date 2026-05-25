# Automatic Plotting for \`SurvivalQuantities“

Automatic Plotting for \`SurvivalQuantities“

## Usage

``` r
# S3 method for class 'SurvivalQuantities'
autoplot(object, conf.level = 0.95, add_km = FALSE, add_wrap = TRUE, ...)
```

## Arguments

- object:

  (`SurvivalQuantities`) survival quantities.

- conf.level:

  (`numeric`) confidence level of the interval. If values of `FALSE`,
  `NULL` or `0` are provided then confidence regions will not be added
  to the plot

- add_km:

  (`logical`) if `TRUE` Kaplan-Meier curves will be added to the plot
  for each group/subject.

- add_wrap:

  (`logical`) if `TRUE` will apply a
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  to the plot by each group/subject.

- ...:

  not used.

## See also

Other SurvivalQuantities:
[`SurvivalQuantities-class`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md),
[`as.data.frame.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.SurvivalQuantities.md),
[`brierScore.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/brierScore.SurvivalQuantities.md),
[`summary.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/summary.SurvivalQuantities.md)

Other autoplot:
[`autoplot`](https://genentech.github.io/jmpost/reference/autoplot.md),
[`autoplot.LongitudinalQuantities()`](https://genentech.github.io/jmpost/reference/autoplot.LongitudinalQuantities.md)
