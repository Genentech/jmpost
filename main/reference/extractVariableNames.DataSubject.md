# Extract Mapping to Standardised Variable Names

Extract a `list` that maps the variable names in a user-defined
`data.frame` to standardised values.

## Usage

``` r
# S3 method for class 'DataSubject'
extractVariableNames(object)
```

## Arguments

- object:

  ([`DataSubject`](https://genentech.github.io/jmpost/reference/DataSubject-class.md))\
  subject-level data.

## Value

A list with the following named elements:

- `subject` (`character`)\
  the name of the variable containing the subject identifier.

- `arm` (`character`)\
  the name of the variable containing the arm identifier.

- `study` (`character`)\
  the name of the variable containing the study identifier.

## See also

Other DataSubject:
[`DataSubject-class`](https://genentech.github.io/jmpost/reference/DataSubject-class.md),
[`as.data.frame.DataSubject()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataSubject.md),
[`as_print_string.DataSubject()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSubject.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md)

Other extractVariableNames:
[`extractVariableNames()`](https://genentech.github.io/jmpost/reference/extractVariableNames.md),
[`extractVariableNames.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataLongitudinal.md),
[`extractVariableNames.DataSurvival()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSurvival.md)
