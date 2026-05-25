# Resolve any promises

Loops over all components and ensures that any
[`PromiseLinkComponent`](https://genentech.github.io/jmpost/reference/PromiseLinkComponent-class.md)
objects are resolved to
[`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
objects.

## Usage

``` r
# S3 method for class 'Link'
resolvePromise(object, model, ...)
```

## Arguments

- object:

  (`Link`) a link object.

- model:

  (`LongitudinalModel`) the model object.

- ...:

  Not Used.
