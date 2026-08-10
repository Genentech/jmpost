# Subject Data Object and Constructor Function

The `DataSubject` class handles the processing of the subject data for
fitting a
[`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).

## Usage

``` r
DataSubject(data, subject, arm, study)
```

## Arguments

- data:

  (`data.frame`) the subject-level data.

- subject:

  (`character`) the name of the variable containing the subject
  identifier.

- arm:

  (`character`) the name of the variable containing the arm identifier.

- study:

  (`character`) the name of the variable containing the study
  identifier.

## Value

A `DataSubject` object.

## Slots

- `data`:

  (`data.frame`)\
  the subject-level data.

- `subject`:

  (`character`)\
  the name of the variable containing the subject identifier.

- `arm`:

  (`character`)\
  the name of the variable containing the arm identifier.

- `study`:

  (`character`)\
  the name of the variable containing the study identifier.

## See also

Other DataObjects:
[`DataJoint-class`](https://genentech.github.io/jmpost/reference/DataJoint-class.md),
[`DataLongitudinal-class`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md),
[`DataSurvival-class`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md)

Other DataSubject:
[`as.data.frame.DataSubject()`](https://genentech.github.io/jmpost/reference/as.data.frame.DataSubject.md),
[`as_print_string.DataSubject()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSubject.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`extractVariableNames.DataSubject()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSubject.md)

## Examples

``` r
dat <- transform(os_data, study = "Study 1")
DataSubject(dat, subject = "id", arm = "arm", study = "study")
#> 
#>    Subject-Data Object:
#>       # of Subjects = 203
#>       # of Studies  = 1
#>       # of Arms     = 2 
#> 
```
