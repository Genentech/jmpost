# `LongitudinalQuantities` Object & Constructor Function

Constructor function to generate a `LongitudinalQuantities` object.

## Usage

``` r
LongitudinalQuantities(object, grid)
```

## Arguments

- object:

  ([`JointModelSamples`](https://genentech.github.io/jmpost/reference/JointModelSamples-class.md))\
  samples as drawn from a Joint Model.

- grid:

  (`Grid`)\
  object that specifies which subjects and time points to calculate the
  quantities for. See
  [Grid-Functions](https://genentech.github.io/jmpost/reference/Grid-Functions.md).

## Details

Note that unlike
[`SurvivalQuantities`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md),
`LongitudinalQuantities` does not support group aggregation.

## Slots

- `quantities`:

  (`Quantities`)\
  The sampled quantities. Should contain 1 element per element of
  `group`

- `data`:

  (`DataJoint`)\
  Survival and Longitudinal Data.

## See also

Other LongitudinalQuantities:
[`as.data.frame.LongitudinalQuantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.LongitudinalQuantities.md),
[`autoplot.LongitudinalQuantities()`](https://genentech.github.io/jmpost/reference/autoplot.LongitudinalQuantities.md),
[`summary.LongitudinalQuantities()`](https://genentech.github.io/jmpost/reference/summary.LongitudinalQuantities.md)
