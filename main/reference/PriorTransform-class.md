# Package-Defined Prior Transformation

Describes a transformation from a model parameter to the scale on which
a prior density is evaluated. Only package-defined transformations are
supported; the object contains no user-supplied R or Stan functions.

## Slots

- `kind`:

  (`character`)\
  Transformation identifier.

- `scale`:

  (`numeric`)\
  Positive multiplier inside the exponential.

## See also

[`prior_transform_exp()`](https://genentech.github.io/jmpost/reference/prior-transformations.md),
[`prior_transform_square()`](https://genentech.github.io/jmpost/reference/prior-transformations.md)
