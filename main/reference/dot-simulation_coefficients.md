# Normalise simulation coefficients to a design matrix

An empty vector represents zero coefficients. For convenience, a full
set of dummy coefficients may be supplied when its reference-level
coefficient is zero; that leading zero is removed to match reference
coding.

## Usage

``` r
.simulation_coefficients(coefficients, n, argument)
```

## Arguments

- coefficients:

  Numeric vector of simulation coefficients.

- n:

  Required number of design-matrix coefficients.

- argument:

  Name of the user-facing argument, used in error messages.

## Value

A numeric coefficient vector of length `n`.
