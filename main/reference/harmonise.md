# Prepare Data Object

Prepare Data Object

## Usage

``` r
# S3 method for class 'DataSubject'
harmonise(object, ...)

harmonise(object, ...)

# Default S3 method
harmonise(object, ...)

# S3 method for class 'DataLongitudinal'
harmonise(object, subject_var, subject_ord, ...)

# S3 method for class 'DataSurvival'
harmonise(object, subject_var, subject_ord, ...)
```

## Arguments

- object:

  (`DataSubject` or `DataLongitudinal` or `DataSurvival`)\
  data object to "harmonise"

- ...:

  not used.

- subject_var:

  (`character`)\
  the name of the variable containing the subject identifier.

- subject_ord:

  (`character`)\
  the expected levels (in order) of the subject identifier.

## Value

Returns the original object but with the data standardised (see details)

## Details

This utility function prepares the datasets in the data objects in order
to ensure they are consistent and compatible with each other.

In particular it ensures that the `subject` variable, as specified by
`DataSubject`, is available in `DataLongitudinal` and `DataSurvival` and
that all levels are present in all 3 data objects.

It also sorts the datasets to ensure that indexes are consistent e.g.
index 1 for `DataSubject@data` corresponds to the same subject as index
1 for `DataSurvival@data`. For `DataLongitudinal` the data is
additionally sorted by time and outcome value.

## See also

[`DataJoint`](https://genentech.github.io/jmpost/reference/DataJoint-class.md),
[`DataSurvival`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md),
[`DataSubject`](https://genentech.github.io/jmpost/reference/DataSubject-class.md),
[`DataLongitudinal`](https://genentech.github.io/jmpost/reference/DataLongitudinal-class.md)
