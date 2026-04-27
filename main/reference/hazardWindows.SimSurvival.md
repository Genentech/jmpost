# Construct Time Intervals

Construct Time Intervals

## Usage

``` r
# S3 method for class 'SimSurvival'
hazardWindows(object, ...)
```

## Arguments

- object:

  (`SimSurvival`)\
  the survival simulation object to create evaluation points for.

- ...:

  Not Used.

## Value

A `tibble` with `lower`, `upper`, `time`, `eval` and `width`.
