# Extract Mapping to Standardised Variable Names

Extract a `list` that maps the variable names in a user-defined
`data.frame` to standardised values.

## Usage

``` r
# S3 method for class 'DataSurvival'
extractVariableNames(object)
```

## Arguments

- object:

  ([`DataSurvival`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md))\
  Survival Data.

## Value

A list with the following named elements:

- `frm` (`formula`)\
  a symbolic description of the survival model to be fitted

- `time` (`character`)\
  The name of the variable containing the event time

- `event` (`character`)\
  The name of the variable containing the event status

## See also

Other DataSurvival:
[`DataSurvival-class`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md),
[`as.data.frame.DataSurvival()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataSurvival.md),
[`as_print_string.DataSurvival()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSurvival.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md)

Other extractVariableNames:
[`extractVariableNames()`](https://genentech.github.io/jmpost/reference/extractVariableNames.md),
[`extractVariableNames.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataLongitudinal.md),
[`extractVariableNames.DataSubject()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSubject.md)
