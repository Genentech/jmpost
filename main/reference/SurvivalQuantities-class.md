# `SurvivalQuantities` Object & Constructor Function

Constructor function to generate a `SurvivalQuantities` object.

## Usage

``` r
SurvivalQuantities(object, grid, type = c("surv", "haz", "loghaz", "cumhaz"))
```

## Arguments

- object:

  (`JointModelSamples`) samples as drawn from a Joint Model.

- grid:

  (`Grid`) object that specifies which subjects and time points to
  calculate the quantities for. See
  [Grid-Functions](https://genentech.github.io/jmpost/reference/Grid-Functions.md).

- type:

  (`character`) quantity to be generated. Must be one of `surv`, `haz`,
  `loghaz`, `cumhaz`.

## Slots

- `quantities`:

  (`Quantities`)\
  The sampled quantities. Should contain 1 element per element of
  `group`

- `groups`:

  (`list`)\
  See argument section for details

- `type`:

  (`character`)\
  See See argument section for details

- `time_grid`:

  (`numeric`)\
  See argument section for details

- `data`:

  ([`DataJoint`](https://genentech.github.io/jmpost/reference/DataJoint-class.md))\
  The data that the Joint Model was fitted to to produce the
  samples/quantities

## Group Specification

If `groups` is a character vector of subject IDs then the survival
quantities will only be calculated for those specific subjects.

If `groups` is a list then any elements with more than 1 subject ID will
be grouped together and their quantities will be calculated by taking a
point-wise average. For example:
`groups = list("g1" = c("sub1", "sub2"), "g2" = c("sub3", "sub4"))`
would result in 2 groups being created whose values are the pointwise
average of `c("sub1", "sub2")` and `c("sub3", "sub4")` respectively.

If `groups=NULL` then all subjects from original dataset will be
selected

## See also

Other SurvivalQuantities:
[`as.data.frame.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.SurvivalQuantities.md),
[`autoplot.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/autoplot.SurvivalQuantities.md),
[`brierScore.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/brierScore.SurvivalQuantities.md),
[`summary.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/summary.SurvivalQuantities.md)
