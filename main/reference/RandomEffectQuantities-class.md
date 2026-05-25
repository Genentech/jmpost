# Random Effects Quantities Container

A simple wrapper around a `matrix` to store required metadata for
patient level random effects data

## Usage

``` r
RandomEffectQuantities(quantities, subject, parameter)
```

## Arguments

- quantities:

  (`matrix`) of random effects values.

- subject:

  (`character`) labels specifying which subjects the values belong to.

- parameter:

  (`character`) labels specifying which parameter the value is.

## Details

Each row of the matrix represents a sample and each column represents a
distinct subject specific parameter. As such the number of columns in
the matrix should equal the length of `subject` and `parameter` which
provide metadata for who the parameter corresponds to as well as which
parameter it is.

## Slots

- `quantities`:

  (`matrix`)\
  See Arguments for details.

- `subject`:

  (`numeric`)\
  See Arguments for details.

- `parameter`:

  (`character`)\
  See Arguments for details.

## See also

Other RandomEffectQuantities:
[`LongitudinalRandomEffects()`](https://genentech.github.io/jmpost/reference/LongitudinalRandomEffects.md),
[`as.data.frame.RandomEffectQuantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.RandomEffectQuantities.md),
[`as_print_string.RandomEffectQuantities()`](https://genentech.github.io/jmpost/reference/as_print_string.RandomEffectQuantities.md),
[`summary.RandomEffectQuantities()`](https://genentech.github.io/jmpost/reference/summary.RandomEffectQuantities.md)
