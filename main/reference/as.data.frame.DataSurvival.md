# `DataSurvival` -\> `data.frame`

Converts a
[`DataSurvival`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md)
object into a `data.frame`. The subject variable is cast to factor.

## Usage

``` r
# S3 method for class 'DataSurvival'
as.data.frame(x, ...)
```

## Arguments

- x:

  (`DataSurvival`) Survival Data.

- ...:

  Not Used.

## See also

Other DataSurvival:
[`DataSurvival-class`](https://genentech.github.io/jmpost/reference/DataSurvival-class.md),
[`as_print_string.DataSurvival()`](https://genentech.github.io/jmpost/reference/as_print_string.DataSurvival.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`extractVariableNames.DataSurvival()`](https://genentech.github.io/jmpost/reference/extractVariableNames.DataSurvival.md)
