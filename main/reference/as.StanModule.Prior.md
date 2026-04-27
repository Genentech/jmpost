# `Prior` -\> `StanModule`

Converts a
[`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md)
object to a
[`StanModule`](https://genentech.github.io/jmpost/reference/StanModule-class.md)
object

## Usage

``` r
# S3 method for class 'Prior'
as.StanModule(object, name, ...)
```

## Arguments

- object:

  ([`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md))\
  a prior Distribution

- name:

  (`character`)\
  the name of the parameter the prior distribution is for

- ...:

  Not Used.

## See also

Other Prior-internal:
[`Prior-Getter-Methods`](https://genentech.github.io/jmpost/reference/Prior-Getter-Methods.md),
[`Prior-class`](https://genentech.github.io/jmpost/reference/Prior-class.md),
[`as.character.Prior()`](https://genentech.github.io/jmpost/reference/as.character.Prior.md),
[`as_stan_list.Prior()`](https://genentech.github.io/jmpost/reference/as_stan_list.Prior.md)

Other as.StanModule:
[`as.StanModule()`](https://genentech.github.io/jmpost/reference/as.StanModule.md),
[`as.StanModule.JointModel()`](https://genentech.github.io/jmpost/reference/as.StanModule.JointModel.md),
[`as.StanModule.Link()`](https://genentech.github.io/jmpost/reference/as.StanModule.Link.md),
[`as.StanModule.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.StanModule.LinkComponent.md),
[`as.StanModule.Parameter()`](https://genentech.github.io/jmpost/reference/as.StanModule.Parameter.md),
[`as.StanModule.ParameterList()`](https://genentech.github.io/jmpost/reference/as.StanModule.ParameterList.md)
