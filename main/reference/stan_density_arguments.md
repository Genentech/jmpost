# Split a Stan Density Argument List

Splits a comma-separated Stan argument list at its outermost level,
retaining commas inside nested function calls and indexing expressions.

## Usage

``` r
stan_density_arguments(code)
```

## Arguments

- code:

  (`string`) Contents of a Stan function's argument list, without its
  enclosing parentheses. Empty or whitespace-only input is allowed.

## Value

A character vector with one element per argument, or `character(0)` for
an empty or whitespace-only argument list.

## Details

Leading and trailing whitespace is removed from each argument; internal
whitespace, argument order and repeated arguments are preserved. Nesting
is determined by parentheses and square brackets, including
multidimensional indexes. The input is assumed to contain well-formed
expressions with balanced delimiters. String literals, comments and
brace-delimited array literals are not parsed.

## See also

[`stan_prior_density()`](https://genentech.github.io/jmpost/reference/stan_prior_density.md)
