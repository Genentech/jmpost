# `ParameterList` -\> `list`

Returns a named list where each element of the list corresponds to a
Stan modelling block e.g. `data`, `model`, etc.

## Usage

``` r
# S3 method for class 'ParameterList'
as.list(x, ...)
```

## Arguments

- x:

  (`ParameterList`)\
  A List of
  [`Parameter`](https://genentech.github.io/jmpost/reference/Parameter-class.md)
  Objects.

- ...:

  Not Used.

## See also

Other ParameterList:
[`ParameterList-Getter-Methods`](https://genentech.github.io/jmpost/reference/ParameterList-Getter-Methods.md),
[`ParameterList-class`](https://genentech.github.io/jmpost/reference/ParameterList-class.md),
[`as.StanModule.ParameterList()`](https://genentech.github.io/jmpost/reference/as.StanModule.ParameterList.md),
[`as_print_string.ParameterList()`](https://genentech.github.io/jmpost/reference/as_print_string.ParameterList.md),
[`as_stan_list.ParameterList()`](https://genentech.github.io/jmpost/reference/as_stan_list.ParameterList.md)
