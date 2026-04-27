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

  ([`Link`](https://genentech.github.io/jmpost/reference/Link-class.md))\
  a link object.

- model:

  ([`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md))\
  the model object.

- ...:

  Not Used.
