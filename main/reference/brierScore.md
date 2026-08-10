# `brierScore`

Returns the Brier Score for a given model

## Usage

``` r
brierScore(object, ...)
```

## Arguments

- object:

  to calculate Brier Score for.

- ...:

  additional options.

## Value

A numeric Brier score or a `data.frame` of scores over time.

## See also

Other brierScore:
[`brierScore.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/brierScore.SurvivalQuantities.md)

## Examples

``` r
if (FALSE) { # \dontrun{
brierScore(survival_quantities)
} # }
```
