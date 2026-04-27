# `StanModule` -\> `list`

Returns a named list where each element of the list corresponds to a
Stan modelling block e.g. `data`, `model`, etc.

## Usage

``` r
# S3 method for class 'StanModule'
as.list(x, stan_blocks = STAN_BLOCKS, ...)
```

## Arguments

- x:

  ([`StanModule`](https://genentech.github.io/jmpost/reference/StanModule-class.md))\
  A Stan Module

- stan_blocks:

  (`list`)\
  reference list of stan blocks.

- ...:

  Not Used.

## See also

Other StanModule:
[`StanModule-class`](https://genentech.github.io/jmpost/reference/StanModule-class.md),
[`as.character.StanModule()`](https://genentech.github.io/jmpost/reference/as.character.StanModule.md),
[`as_print_string.StanModule()`](https://genentech.github.io/jmpost/reference/as_print_string.StanModule.md)
