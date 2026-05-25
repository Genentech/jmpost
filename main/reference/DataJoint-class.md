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
