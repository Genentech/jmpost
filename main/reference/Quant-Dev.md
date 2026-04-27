# Quantity Developer Notes

Developer details for `QuantityX` objects/methods. This page just
outlines the arguments and slots of these objects/methods. For the full
implementation details please see
[Grid-Dev](https://genentech.github.io/jmpost/reference/Grid-Dev.md)

## Usage

``` r
as.QuantityGenerator(object, ...)

as.QuantityCollapser(object, ...)

QuantityCollapser(times, groups, indexes)

# S3 method for class 'GridEven'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridEven'
as.QuantityCollapser(object, data, ...)

# S3 method for class 'GridEvent'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridEvent'
as.QuantityCollapser(object, data, ...)

# S3 method for class 'GridFixed'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridFixed'
as.QuantityCollapser(object, data, ...)

# S3 method for class 'GridGrouped'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridGrouped'
as.QuantityCollapser(object, data, ...)

# S3 method for class 'GridManual'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridManual'
as.QuantityCollapser(object, data, ...)

# S3 method for class 'GridObserved'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridObserved'
as.QuantityCollapser(object, data, ...)

# S3 method for class 'GridPopulation'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridPopulation'
as.QuantityCollapser(object, data, ...)

# S3 method for class 'GridPrediction'
as.QuantityGenerator(object, data, ...)

# S3 method for class 'GridPrediction'
as.QuantityCollapser(object, data, ...)

QuantityGeneratorPopulation(times, studies = NULL, arms = NULL)

QuantityGeneratorPrediction(times, newdata = NULL, params = NULL)

QuantityGeneratorSubject(times, subjects = NULL)
```

## Arguments

- object:

  (`Grid`)\
  object to convert to a `QuantityGenerator` or `QuantityCollapser`.

- ...:

  Not currently used.

- times:

  (`numeric`)\
  vector of time points to extract quantities at.

- groups:

  (`character`)\
  vector of labels to apply to the generated quantities.

- indexes:

  (`list`)\
  list of indexes that specify which observations from a
  `QuantityGenerator` should be combined to form the desired quantities.

- data:

  (`DataJoint`)\
  Survival and Longitudinal Data.

- subjects:

  (`character`)\
  vector of subjects to extract quantities for.

## Details

The `as.QuantityGenerator` must return a `QuantityGenerator` object. The
`as.QuantityCollapser` must return a `QuantityCollapser` object.

## Slots

- `times`:

  (`numeric`)\
  See Arguments for details.

- `subjects`:

  (`character`)\
  See Arguments for details.

- `groups`:

  (`character`)\
  See Arguments for details.

- `indexes`:

  (`list`)\
  See Arguments for details.
