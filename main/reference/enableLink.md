# Enable Link Generic

Enable Link Generic

## Usage

``` r
enableLink(object, ...)
```

## Arguments

- object:

  (`LongitudinalModel`) to enable link for.

- ...:

  Not used.

  Optional hook method that is called on a
  [`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  only if a link method is provided to
  [`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).
  This can be used to allow the model to include any optional stan code
  that is only required if there are links present.

## Value

[`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
object
