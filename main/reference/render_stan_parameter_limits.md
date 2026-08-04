# Render Stan Parameter Constraints

Converts a lower/upper limit vector from a
[`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md)
into a Stan declaration constraint, e.g. `<lower=0.1, upper=1>`.

## Usage

``` r
render_stan_parameter_limits(limits)
```

## Arguments

- limits:

  (`numeric`) length-two vector containing lower and upper parameter
  limits.

## Value

A length-one character vector.
