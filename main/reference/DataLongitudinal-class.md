# Longitudinal Data Object and Constructor Function

The `DataLongitudinal` class handles the processing of the longitudinal
data for fitting a Joint Model.

## Usage

``` r
DataLongitudinal(data, formula, threshold = NULL)
```

## Arguments

- data:

  (`data.frame`) containing the observed longitudinal data.

- formula:

  (`formula`) of the form `outcome ~ time`, and cannot contain any
  additional covariates.

- threshold:

  (`numeric`) cut-off value to be used to declare an observation as
  censored (below detection limit).

## Slots

- `data`:

  (`data.frame`)\
  See Arguments for details; Note that observations that contain missing
  values in the required variables are removed.

- `formula`:

  (`formula`)\
  See Arguments for details

- `threshold`:

  (`numeric`)\
  See Arguments for details

## See also

Other DataObjects:
[`DataJoint-class`](https://genentech.github.io/jmpost/reference/DataJoint-class.md),
[`DataSubject-class`](https://genentech.github.io/jmpost/reference/DataSubject-class.md),
[`DataSurvival-class`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md)

Other DataLongitudinal:
[`as.data.frame.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataLongitudinal.md),
[`as_print_string.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/as_print_string.DataLongitudinal.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`extractVariableNames.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataLongitudinal.md)
