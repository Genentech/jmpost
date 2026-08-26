# As Formula

Utility wrapper function to convert an object to a formula.

## Usage

``` r
as_formula(x, ...)
```

## Arguments

- x:

  (`ANY`) object to convert to a formula.

- ...:

  Not used.

## Value

A `formula` object.

## Examples

``` r
as_formula("response ~ time")
#> response ~ time
#> <environment: 0x560c8c1efab8>
```
