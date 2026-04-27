# Match Order

Match Order

## Usage

``` r
match_order(x)
```

## Arguments

- x:

  (`numeric`)\
  a vector for which we want to generate an index so other vectors can
  be put into the same sort order

  Assuming we have a vector that is sorted then this function will
  return the index vector to convert that sorted vector into the same
  sort order as the input vector `x`. For example let `x = 8, 7, 9 , 7`.
  If sorted we would get `x_sort = 7, 7, 8, 9`. So in order to convert
  `x_sort` back into `x` we'd need an index vector of `3, 1, 4, 2`. This
  function is used to determine that corresponding index vector for an
  arbitrarily sorted vector `x`.

  There is no specific handling of ties. It is assuming that in the case
  of ties for `x` that the vector you are re-indexing also has tied
  values thus the specific tied element selection does not matter.
