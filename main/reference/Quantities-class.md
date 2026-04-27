# Generated Quantities Container

A simple wrapper around a `matrix` to store required metadata

## Usage

``` r
Quantities(quantities, times, groups)
```

## Arguments

- quantities:

  (`matrix`)\
  of generated quantities.

- times:

  (`numeric`)\
  labels specifying which time point the quantity was generated at.

- groups:

  (`character`)\
  labels for which group the quantity belongs to.

## Details

Each row of the matrix represents a sample and each column represents a
distinct quantity. As such the number of columns in the matrix should
equal the length of `times` and `groups` which provide metadata for who
the quantity belongs to and at what time point it was generated at.

## Slots

- `quantities`:

  (`matrix`)\
  See Arguments for details.

- `times`:

  (`numeric`)\
  See Arguments for details.

- `groups`:

  (`character`)\
  See Arguments for details.

## See also

Other Quantities:
[`as.data.frame.Quantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.Quantities.md),
[`as_print_string.Quantities()`](https://genentech.github.io/jmpost/reference/as_print_string.Quantities.md),
[`summary.Quantities()`](https://genentech.github.io/jmpost/reference/summary.Quantities.md)
