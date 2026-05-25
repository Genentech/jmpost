# Survival Data Object and Constructor Function

The `DataSurvival` class handles the processing of the survival data for
fitting a
[`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).

## Usage

``` r
DataSurvival(data, formula)
```

## Arguments

- data:

  (`data.frame`) the observed time-to-event data.

- formula:

  (`formula`) of the form `Surv(time, event) ~ cov1 + cov2 + ...`. See
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) for
  more details, though note that this package only supports right
  censoring.

## Slots

- `data`:

  (`data.frame`)\
  See Arguments for details.

- `formula`:

  (`formula`)\
  See Arguments for details.

## See also

Other DataObjects:
[`DataJoint-class`](https://genentech.github.io/jmpost/reference/DataJoint-class.md),
[`DataLongitudinal-class`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md),
[`DataSubject-class`](https://genentech.github.io/jmpost/reference/DataSubject-class.md)

Other DataSurvival:
[`as.data.frame.DataSurvival()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataSurvival.md),
[`as_print_string.DataSurvival()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSurvival.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`extractVariableNames.DataSurvival()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSurvival.md)
