# `DataSubject` -\> `data.frame`

Converts a
[`DataSubject`](https://genentech.github.io/jmpost/reference/DataSubject-class.md)
object into a `data.frame`. The subject variable is cast to factor.

## Usage

``` r
# S3 method for class 'DataSubject'
as.data.frame(x, ...)
```

## Arguments

- x:

  ([`DataSubject`](https://genentech.github.io/jmpost/reference/DataSubject-class.md))\
  subject-level data.

- ...:

  Not Used.

## See also

Other DataSubject:
[`DataSubject-class`](https://genentech.github.io/jmpost/reference/DataSubject-class.md),
[`as_print_string.DataSubject()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSubject.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`extractVariableNames.DataSubject()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSubject.md)
