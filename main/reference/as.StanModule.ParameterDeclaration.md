# `Parameter` Declaration -\> `StanModule`

Creates only the Stan declaration for a
[`Parameter`](https://genentech.github.io/jmpost/reference/Parameter-class.md).
Sampled parameters are declared in the `parameters` block; constant
parameters are declared in the `transformed parameters` block.

## Usage

``` r
# S3 method for class 'ParameterDeclaration'
as.StanModule(object, ...)
```

## Arguments

- object:

  (`Parameter`) a prior Distribution

- ...:

  not used.

## Value

A
[`StanModule`](https://genentech.github.io/jmpost/reference/StanModule-class.md)
object.
