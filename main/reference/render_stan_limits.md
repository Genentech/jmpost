# Creates Stan Syntax for Truncated distributions

This function creates the Stan syntax for truncated distributions

## Usage

``` r
render_stan_limits(object)

# S3 method for class 'Prior'
render_stan_limits(object)

# S3 method for class 'numeric'
render_stan_limits(object)
```

## Arguments

- object:

  (`Prior | numeric`) prior or lower and upper limits for a truncated
  distribution.

## Value

(`character`) the Stan syntax for truncated distributions

## Methods (by class)

- `render_stan_limits(Prior)`: method for
  [Prior](https://genentech.github.io/jmpost/reference/Prior-class.md)
  objects.

- `render_stan_limits(numeric)`: method for numeric vectors.
