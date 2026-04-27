# Parameter-List Getter Functions

Getter functions for the slots of a
[`ParameterList`](https://genentech.github.io/jmpost/reference/ParameterList-class.md)
object

## Usage

``` r
# S3 method for class 'ParameterList'
names(x)

# S3 method for class 'ParameterList'
initialValues(object, n_chains, ...)

# S3 method for class 'ParameterList'
size(object)
```

## Arguments

- x:

  (`ParameterList`)\
  A List of
  [`Parameter`](https://genentech.github.io/jmpost/reference/Parameter-class.md)
  Objects.

- object:

  (`ParameterList`)\
  A List of
  [`Parameter`](https://genentech.github.io/jmpost/reference/Parameter-class.md)
  Objects.

- n_chains:

  (`integer`)\
  the number of chains.

- ...:

  Not Used.

## Functions

- `names(ParameterList)`: The parameter-list's parameter names

- `initialValues(ParameterList)`: The parameter-list's parameter initial
  values

- `size(ParameterList)`: The parameter-list's parameter dimensionality

## See also

Other ParameterList:
[`ParameterList-class`](https://genentech.github.io/jmpost/reference/ParameterList-class.md),
[`as.StanModule.ParameterList()`](https://genentech.github.io/jmpost/reference/as.StanModule.ParameterList.md),
[`as.list.ParameterList()`](https://genentech.github.io/jmpost/reference/as.list.ParameterList.md),
[`as_print_string.ParameterList()`](https://genentech.github.io/jmpost/reference/as_print_string.ParameterList.md),
[`as_stan_list.ParameterList()`](https://genentech.github.io/jmpost/reference/as_stan_list.ParameterList.md)
