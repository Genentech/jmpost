# Resolve a `PromiseLinkComponent`

Resolves a
[`PromiseLinkComponent`](https://genentech.github.io/jmpost/reference/PromiseLinkComponent-class.md)
object to a
[`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
object. An error will be thrown if the returned
[`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
object does not have the same `key` slot value as the original
[`PromiseLinkComponent`](https://genentech.github.io/jmpost/reference/PromiseLinkComponent-class.md).

## Usage

``` r
# S3 method for class 'PromiseLinkComponent'
resolvePromise(object, model, ...)
```

## Arguments

- object:

  ([`PromiseLinkComponent`](https://genentech.github.io/jmpost/reference/PromiseLinkComponent-class.md))\
  the promise to resolve

- model:

  ([`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md))\
  the model to resolve the promise with

- ...:

  Not used.

## Value

([`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md))\
the resolved `LinkComponent` object
