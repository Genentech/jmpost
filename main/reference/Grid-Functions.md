# Quantity Grid Specification

These functions are used to specify which subjects and timepoints should
be generated when calculating quantities via
[`SurvivalQuantities`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md)
and
[`LongitudinalQuantities`](https://genentech.github.io/jmpost/reference/LongitudinalQuantities-class.md).

## Usage

``` r
GridEven(subjects = NULL, length.out = 30)

GridEvent(subjects = NULL)

GridFixed(subjects = NULL, times = NULL)

GridGrouped(groups, times = NULL)

GridManual(spec)

GridObserved(subjects = NULL)

GridPopulation(times = NULL)

GridPrediction(times = NULL, newdata, params = list())
```

## Arguments

- subjects:

  (`character` or `NULL`)\
  vector of subjects to extract quantities for. If `NULL` will default
  to all subjects within the dataset.

- length.out:

  (`numeric`)\
  number of evenly spaced timepoints to generate quantities at.

- times:

  (`numeric` or `NULL`)\
  vector of time points to extract quantities at. If `NULL` will default
  to 201 evenly spaced timepoints between 0 and either the max
  observation time (for
  [`LongitudinalQuantities`](https://genentech.github.io/jmpost/reference/LongitudinalQuantities-class.md))
  or max event time (for
  [`SurvivalQuantities`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md)).

- groups:

  (`list`)\
  named list of subjects to extract quantities for. See Group
  Specification.

- spec:

  (`list`)\
  named list of subjects to extract quantities for. The names of each
  element should be the required subjects with the element itself being
  a numeric vector of timepoints to generate the quantity at.

- newdata:

  (`data.frame`)\
  new data to generate quantities for. Must contain the same columns and
  factor levels of the original data used in the
  [`DataSurvival`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md)
  object.

- params:

  (`list`)\
  named list of parameters to fix the longitudinal model parameters at
  when predicting survival quantities. See
  [`getPredictionNames()`](https://genentech.github.io/jmpost/reference/getPredictionNames.md)
  for the required parameters.

## Details

- `GridFixed()` is used to specify a fixed set of timepoints to generate
  quantities at for all the specified subjects.

- `GridGrouped()` is similar to `GridFixed()` but allows for groupwise
  averaging (see Group Specification).

- `GridObserved()` generates quantities at the observed longitudinal
  timepoints for each subject.

- `GridManual()` allows for individual timepoint specification for each
  subject.

- `GridEven()` generates quantities for each subject at N evenly spaced
  timepoints between each subjects first and last longitudinal
  observations.

- `GridEvent()` generates one quantity for each subject at their
  event/censor time as indicated by the `time` variable in the survival
  dataset.

- `GridPopulation()` generates longitudinal model quantities based on
  the population parameters at the specified time points. Generates 1
  set of quantities for each distinct combination of `arm` and `study`
  within the
  [`DataSubject`](https://genentech.github.io/jmpost/reference/DataSubject-class.md)
  object provided to the
  [`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).

- `GridPrediction()` generates survival quantities based on any
  user-defined values at the specified time points. This is useful for
  generating quantities for a new dataset on specific longitudinal model
  parameters. See
  [`getPredictionNames()`](https://genentech.github.io/jmpost/reference/getPredictionNames.md)
  to determine which longitudinal model parameters need to be defined
  for a given longitudinal model.

## Group Specification

For `GridGrouped()`, `groups` must be a named list of character vectors.
Each element of the list must be a character vector of the subjects that
will form the group where the element name is the corresponding name of
the group. For example if the goal was to create two groups named
`Group-1` and `Group-2` which are composed of the subjects `sub-1`,
`sub-2` and `sub-3`, `sub-4` respectively then this would be specified
as:

    GridGrouped(
        groups = list(
            "Group-1" = c("sub-1", "sub-2"),
            "Group-2" = c("sub-3", "sub-4")
        )
    )

## See also

[`SurvivalQuantities`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md),
[`LongitudinalQuantities`](https://genentech.github.io/jmpost/reference/LongitudinalQuantities-class.md)
