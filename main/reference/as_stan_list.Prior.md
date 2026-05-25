# `Prior` -\> `list`

Converts a Prior object to a list of parameter data values for a Stan
model.

## Usage

``` r
# S3 method for class 'Prior'
as_stan_list(object, name, ...)
```

## Arguments

- object:

  (`Prior`) a prior Distribution

- name:

  (`character`) the name of the parameter the prior distribution is for

- ...:

  Not Used.

## See also

Other as_stan_list:
[`as_stan_list()`](https://genentech.github.io/jmpost/reference/as_stan_list.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md),
[`as_stan_list.Parameter()`](https://genentech.github.io/jmpost/reference/as_stan_list.Parameter.md),
[`as_stan_list.ParameterList()`](https://genentech.github.io/jmpost/reference/as_stan_list.ParameterList.md)

Other Prior-internal:
[`Prior-Getter-Methods`](https://genentech.github.io/jmpost/reference/Prior-Getter-Methods.md),
[`Prior-class`](https://genentech.github.io/jmpost/reference/Prior-class.md),
[`as.StanModule.Prior()`](https://genentech.github.io/jmpost/reference/as.StanModule.Prior.md),
[`as.character.Prior()`](https://genentech.github.io/jmpost/reference/as.character.Prior.md)
