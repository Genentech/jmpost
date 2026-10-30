# Render Stan Parameter Declaration

Creates a Stan declaration for a sampled scalar or vector parameter.

## Usage

``` r
render_stan_parameter_declaration(name, size, limits, multiplier = "")
```

## Arguments

- name:

  (`string`) parameter name.

- size:

  (`numeric_OR_character`) parameter size.

- limits:

  (`numeric`) lower and upper parameter limits.

- multiplier:

  (`string`) Stan affine multiplier expression for an unbounded
  parameter.

## Value

A length-one character vector.
