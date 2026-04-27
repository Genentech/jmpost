# Data Object -\> `list`

Coerces a data object into a `list` of data components required for
fitting a
[`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).
See the "Extending jmpost" vignette for more details.

## Usage

``` r
# S3 method for class 'DataSubject'
as_stan_list(object, ...)

# S3 method for class 'DataSubject'
as.list(x, ...)

# S3 method for class 'DataLongitudinal'
as_stan_list(object, subject_var, ...)

# S3 method for class 'DataLongitudinal'
as.list(x, ...)

# S3 method for class 'DataSurvival'
as_stan_list(object, ...)

# S3 method for class 'DataSurvival'
as.list(x, ...)

# S3 method for class 'DataJoint'
as_stan_list(object, ...)

# S3 method for class 'DataJoint'
as.list(x, ...)
```

## Arguments

- object:

  (`DataSubject` or `DataLongitudinal` or `DataSurvival`)\
  data object to convert to a `list`.

- ...:

  not used.

- x:

  (`DataSubject` or `DataLongitudinal` or `DataSurvival`)\
  data object to convert to a `list`.

- subject_var:

  (`character`)\
  the name of the variable containing the subject identifier.

## See also

Other DataSubject:
[`DataSubject-class`](https://genentech.github.io/jmpost/reference/DataSubject-class.md),
[`as.data.frame.DataSubject()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataSubject.md),
[`as_print_string.DataSubject()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSubject.md),
[`extractVariableNames.DataSubject()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSubject.md)

Other DataLongitudinal:
[`DataLongitudinal-class`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md),
[`as.data.frame.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataLongitudinal.md),
[`as_print_string.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/as_print_string.DataLongitudinal.md),
[`extractVariableNames.DataLongitudinal()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataLongitudinal.md)

Other DataSurvival:
[`DataSurvival-class`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md),
[`as.data.frame.DataSurvival()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataSurvival.md),
[`as_print_string.DataSurvival()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSurvival.md),
[`extractVariableNames.DataSurvival()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSurvival.md)

Other as_stan_list:
[`as_stan_list()`](https://genentech.github.io/jmpost/reference/as_stan_list.md),
[`as_stan_list.Parameter()`](https://genentech.github.io/jmpost/reference/as_stan_list.Parameter.md),
[`as_stan_list.ParameterList()`](https://genentech.github.io/jmpost/reference/as_stan_list.ParameterList.md),
[`as_stan_list.Prior()`](https://genentech.github.io/jmpost/reference/as_stan_list.Prior.md)

Other DataJoint:
[`DataJoint-class`](https://genentech.github.io/jmpost/reference/DataJoint-class.md),
[`subset.DataJoint()`](https://genentech.github.io/jmpost/reference/subset.DataJoint.md)
