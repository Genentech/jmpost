# `Parameter`

Stores the name, the prior distribution and the size of a parameter. If
`size` is a string then this indicates the name of the variable within
the stan data object that specifies the size of this parameter.

## Usage

``` r
Parameter(prior, name, size = 1)
```

## Arguments

- prior:

  (`Prior`) for the parameter.

- name:

  (`string`) of the parameter.

- size:

  (`numeric` or `string`) dimension of the parameter.

## Value

A `Parameter` object.

## Slots

- `name`:

  (`string`)\
  of the parameter.

- `prior`:

  (`Prior`)\
  for the parameter.

- `size`:

  (`numeric` or `string`)\
  dimension of the parameter.

## See also

Other Parameter:
[`Parameter-Getter-Methods`](https://genentech.github.io/jmpost/reference/Parameter-Getter-Methods.md),
[`as.StanModule.Parameter()`](https://genentech.github.io/jmpost/reference/as.StanModule.Parameter.md),
[`as.character.Parameter()`](https://genentech.github.io/jmpost/reference/as.character.Parameter.md),
[`as_stan_list.Parameter()`](https://genentech.github.io/jmpost/reference/as_stan_list.Parameter.md)

## Examples

``` r
Parameter(prior = prior_normal(0, 1), name = "beta")
#> 
#> Parameter Object:
#>    beta ~ normal(mu = 0, sigma = 1)
#> 
```
