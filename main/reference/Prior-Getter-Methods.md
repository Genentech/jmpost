# Prior Getter Functions

Getter functions for the slots of a
[`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md)
object

## Usage

``` r
# S3 method for class 'Prior'
initialValues(object, ...)

# S3 method for class 'Prior'
auxiliaryInitialValues(object, name, size, ...)

# S3 method for class 'Prior'
auxiliarySize(object, name, size, ...)
```

## Arguments

- object:

  (`Prior`) a prior Distribution

- ...:

  Not Used.

- name:

  (`character`) the name of the parameter the prior distribution is for

- size:

  (`numeric` or `character`) the parameter size.

## Value

The requested prior initial values or auxiliary-parameter information.

## Functions

- `initialValues(Prior)`: The prior's initial value

- `auxiliaryInitialValues(Prior)`: The prior's auxiliary initial values

- `auxiliarySize(Prior)`: The prior's auxiliary parameter sizes

## See also

Other Prior-internal:
[`Prior-class`](https://genentech.github.io/jmpost/reference/Prior-class.md),
[`as.StanModule.Prior()`](https://genentech.github.io/jmpost/reference/as.StanModule.Prior.md),
[`as.character.Prior()`](https://genentech.github.io/jmpost/reference/as.character.Prior.md),
[`as_stan_list.Prior()`](https://genentech.github.io/jmpost/reference/as_stan_list.Prior.md)
