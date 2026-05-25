# Replicate Single Values in a List

Replicate Single Values in a List

## Usage

``` r
expand_initial_values(initial_values, sizes)
```

## Arguments

- initial_values:

  (`list`) initial values with names.

- sizes:

  (`list`) each size corresponds to an element in `initial_values`,
  matched by the names. An attribute `array` must be attached to each
  element, see
  [`replace_with_lookup()`](https://genentech.github.io/jmpost/reference/replace_with_lookup.md).

## Value

A named list of values, with any single values in the `initial_values`
list replicated according to the corresponding values in the `sizes`
list. Even when the size is 1, the value is passed as an `array` if the
corresponding attribute is `TRUE` in `sizes`.

## Note

The resulting list has the same names as the original lists.
