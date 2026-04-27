# Promise of a `LinkComponent`

An object that promises to resolve to a
[`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
object. Inheriting from
[`Promise`](https://genentech.github.io/jmpost/reference/Promise-class.md)
and
[`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md).

## Usage

``` r
PromiseLinkComponent(fun, prior, key)
```

## Arguments

- fun:

  (`function`)\
  a function that returns a `LinkComponent`. See details.

- prior:

  (`Prior`)\
  The prior for the scaling coeficient.

- key:

  (`character`)\
  Link identifier. See Details.

## Details

The `fun` slot should be a function of signature
`function(prior, model)` and should return a
[`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
object. An error will be thrown if the returned
[`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
object does not have the same `key` slot value as the original
`PromiseLinkComponent`.

## Slots

- `fun`:

  (`function`)\
  a function that returns a `LinkComponent`. See details.
