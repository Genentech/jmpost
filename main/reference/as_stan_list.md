# `as_stan_list`

Extracts a list of data elements from an object to be used as input to a
Stan Model

## Usage

``` r
as_stan_list(object, ...)

# Default S3 method
as_stan_list(object, ...)
```

## Arguments

- object:

  to be converted.

- ...:

  additional options.

## Value

A named `list` suitable for use as Stan data.

## See also

Other as_stan_list:
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`as_stan_list.Parameter()`](https://genentech.github.io/jmpost/reference/as_stan_list.Parameter.md),
[`as_stan_list.ParameterList()`](https://genentech.github.io/jmpost/reference/as_stan_list.ParameterList.md),
[`as_stan_list.Prior()`](https://genentech.github.io/jmpost/reference/as_stan_list.Prior.md)

## Examples

``` r
as_stan_list(Parameter(prior_normal(0, 1), "beta"))
#> $prior_mu_beta
#> [1] 0
#> 
#> $prior_sigma_beta
#> [1] 1
#> 
```
