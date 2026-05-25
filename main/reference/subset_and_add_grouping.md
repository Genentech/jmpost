# `subset_and_add_grouping`

`subset_and_add_grouping`

## Usage

``` r
subset_and_add_grouping(dat, groupings)
```

## Arguments

- dat:

  (`data.frame`) must have a column called `subject` which corresponds
  to the values passed to `groupings`.

- groupings:

  (`character` or `list`) subjects that you wish to subset the dataset
  to contain. If `groupings` is a list then an additional variable
  `group` will be added onto the dataset specifying which group the row
  belongs to.

## Details

Example of usage

    subjects <- c("SUB1", "SUB2", "SUB3", "SUB4")
    subset_and_add_grouping(dat, subjects)

    groups <- list(
        "g1" = c("SUB1", "SUB3", "SUB4"),
        "g2" = c("SUB2", "SUB3")
      )
    subset_and_add_grouping(dat, groups)
