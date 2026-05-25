# Row Numbers of Data with Missing Variables

Row Numbers of Data with Missing Variables

## Usage

``` r
get_missing_rownumbers(df, formula = NULL)
```

## Arguments

- df:

  (`data.frame`) input data.

- formula:

  (`formula` or `NULL`) which variables to inspect for missingness, if
  `NULL` all variables are considered.

## Value

Numeric vector specifying which rows contain at least 1 missing
observation in any of the inspected variables.
