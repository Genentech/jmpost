# Joint Data Object and Constructor Function

The `DataJoint` class handles combining data from a
[`DataSurvival`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md)
object and a
[`DataLongitudinal`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md)
object.

## Usage

``` r
DataJoint(subject, survival = NULL, longitudinal = NULL)
```

## Arguments

- subject:

  (`DataSubject`) object created by
  [`DataSubject()`](https://genentech.github.io/jmpost/reference/DataSubject-class.md).

- survival:

  (`DataSurvival`) object created by
  [`DataSurvival()`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md).

- longitudinal:

  (`DataLongitudinal`) object created by
  [`DataLongitudinal()`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md).

## Value

A `DataJoint` object.

## Slots

- `subject`:

  (`DataSubject`)\
  See Argument for details.

- `survival`:

  (`DataSurvival`)\
  See Argument for details.

- `longitudinal`:

  (`DataLongitudinal`)\
  See Argument for details.

## See also

Other DataObjects:
[`DataLongitudinal-class`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md),
[`DataSubject-class`](https://genentech.github.io/jmpost/reference/DataSubject-class.md),
[`DataSurvival-class`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md)

Other DataJoint:
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`subset.DataJoint()`](https://genentech.github.io/jmpost/reference/subset.DataJoint.md)

## Examples

``` r
subjects <- data.frame(id = c("1", "2"), arm = "A", study = "S")
survival <- data.frame(id = c("1", "2"), time = c(5, 8), event = c(1, 0))
longitudinal <- data.frame(
  id = rep(c("1", "2"), each = 2),
  time = rep(c(0, 1), 2),
  response = c(10, 9, 12, 11)
)
DataJoint(
  DataSubject(subjects, "id", "arm", "study"),
  DataSurvival(survival, Surv(time, event) ~ 1),
  DataLongitudinal(longitudinal, response ~ time)
)
#> 
#>  Joint-Data Object Containing:
#> 
#>       Subject-Data Object:
#>           # of Subjects = 2
#>           # of Studies  = 1
#>           # of Arms     = 1
#> 
#>       Survival-Data Object:
#>           # of Rows     = 2
#>           # of Columns  = 3
#>           # of Events   = 1
#>           Formula       = Surv(time, event) ~ 1
#> 
#>       Longitudinal-Data Object:
#>           # of Rows     = 4
#>           # of Columns  = 3
#>           # of Cen-Obvs = 0
#>           Formula       = response ~ time 
#> 
```
