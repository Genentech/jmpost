# Extract Mapping to Standardised Variable Names

Extract a `list` that maps the variable names in a user-defined
`data.frame` to standardised values.

## Usage

``` r
# S3 method for class 'DataLongitudinal'
extractVariableNames(object)
```

## Arguments

- object:

  (`DataLongitudinal`) Longitudinal Data.

## Value

A list with the following named elements:

- `subject` (`character`)\
  The name of the variable containing the subject identifier

- `frm` (`formula`)\
  of the form `outcome ~ time`

- `time` (`character`)\
  The name of the variable containing the outcome time

- `outcome` (`character`)\
  The name of the variable containing the outcome values

- `threshold` (`numeric`)\
  cut-off value to be used to declare an observation as censored (below
  detection limit).

## See also

Other DataLongitudinal:
[`DataLongitudinal-class`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md),
[`as.data.frame.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataLongitudinal.md),
[`as_print_string.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/as_print_string.DataLongitudinal.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md)

Other extractVariableNames:
[`extractVariableNames()`](https://genentech.github.io/jmpost/reference/extractVariableNames.md),
[`extractVariableNames.DataSubject()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSubject.md),
[`extractVariableNames.DataSurvival()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSurvival.md)
