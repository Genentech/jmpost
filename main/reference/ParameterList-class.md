# `ParameterList`

This class extends the general
[`list`](https://rdrr.io/r/base/list.html) type for containing
[`Parameter`](https://genentech.github.io/jmpost/reference/Parameter-class.md)
specifications. When converted to a
[`StanModule`](https://genentech.github.io/jmpost/reference/StanModule-class.md),
the list supplies the Stan declarations and prior statements for those
parameters.

## Usage

``` r
ParameterList(...)
```

## Arguments

- ...:

  (`Parameter`) which parameter specifications to include.

## Slots

- `parameters`:

  (`list`)\
  a list of
  [`Parameter`](https://genentech.github.io/jmpost/reference/Parameter-class.md)
  objects

## See also

Other ParameterList:
[`ParameterList-Getter-Methods`](https://genentech.github.io/jmpost/reference/ParameterList-Getter-Methods.md),
[`as.StanModule.ParameterList()`](https://genentech.github.io/jmpost/reference/as.StanModule.ParameterList.md),
[`as.list.ParameterList()`](https://genentech.github.io/jmpost/reference/as.list.ParameterList.md),
[`as_print_string.ParameterList()`](https://genentech.github.io/jmpost/reference/as_print_string.ParameterList.md),
[`as_stan_list.ParameterList()`](https://genentech.github.io/jmpost/reference/as_stan_list.ParameterList.md)
