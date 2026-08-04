# Render Stan Constant Parameter Declaration

Creates a Stan transformed-parameter declaration that fixes a scalar or
vector parameter at a data-supplied constant value.

## Usage

``` r
render_stan_const_declaration(name, size, limits)
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
