# Render Stan Parameter Declaration

Creates a Stan declaration for a sampled scalar or vector parameter.

## Usage

``` r
render_stan_parameter_declaration(name, size, limits)
```

## Arguments

- name:

  (`string`) parameter name.

- size:

  (`numeric_OR_character`) parameter size.

- limits:

  (`numeric`) lower and upper parameter limits.

## Value

A length-one character vector.
