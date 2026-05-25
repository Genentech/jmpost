# Remove Rows with Missing Variables

Removes any rows from a data set that contain missing values in the
inspected variables. Allows users to specify which variables to inspect
for missing values based on either a formula or a character vector of
variable names.

## Usage

``` r
remove_missing_rows(data, formula, extra_vars = NULL)
```

## Arguments

- data:

  (`data.frame`) input data.

- formula:

  (`formula` or `NULL`) which variables to inspect for missingness.

- extra_vars:

  (`character`) additional variables to inspect for missingness.

## Value

The `data` after removing observations that contain missing values in
the required variables. Note that additional variables not listed in
`formula` or `extra_vars` are not dropped and may still contain missing
values.
