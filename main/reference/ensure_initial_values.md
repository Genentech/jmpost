# Ensure that initial values are correctly specified

Ensure that initial values are correctly specified

## Usage

``` r
ensure_initial_values(initial_values, data, parameters)
```

## Arguments

- initial_values:

  (`list`) A list of lists containing the initial values must be 1 list
  per desired chain. All elements should have identical names

- data:

  (`list`) specifies the size to expand each of our initial values to
  be. That is elements of size 1 in `initial_values` will be expanded to
  be the same size as the corresponding element in `data` by
  broadcasting the value.

- parameters:

  (`ParameterList`) the parameters object

## Details

This function is mostly a thin wrapper around `expand_initial_values` to
enable easier unit testing.
