# `validate_time_grid`

Validate that the provided time grid is:

- finite

- numeric

- non-missing

- sorted

- unique

## Usage

``` r
validate_time_grid(time_grid)
```

## Arguments

- time_grid:

  (`numeric`) A vector of times which quantities will be evaluated at.

## Value

Invisibly returns `time_grid`; invalid grids raise an error.
