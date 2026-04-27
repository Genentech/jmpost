# `getParameters`

Extract any modelling parameters as a
[`ParameterList`](https://genentech.github.io/jmpost/reference/ParameterList-class.md)
object from a model.

## Usage

``` r
getParameters(object, ...)

# S3 method for class 'StanModel'
getParameters(object, ...)

# S3 method for class 'LinkComponent'
getParameters(object, ...)

# S3 method for class 'Link'
getParameters(object, ...)

# Default S3 method
getParameters(object, ...)
```

## Arguments

- object:

  where to obtain the parameters from.

- ...:

  additional options.

## See also

Other LinkComponent:
[`LinkComponent-class`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md),
[`as.StanModule.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.StanModule.LinkComponent.md),
[`as.list.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.list.LinkComponent.md),
[`initialValues()`](https://genentech.github.io/jmpost/reference/initialValues.md)
