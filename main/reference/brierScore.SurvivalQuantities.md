# `brierScore`

Derives the Brier Scores (using Inverse Probability of Censoring
Weighting) for the Survival estimates as detailed in (Blanche et al.
2015) .

## Usage

``` r
# S3 method for class 'SurvivalQuantities'
brierScore(object, maintain_cen_order = TRUE, event_offset = TRUE, ...)
```

## Arguments

- object:

  ([`SurvivalQuantities`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md))\
  survival quantities.

- maintain_cen_order:

  (`logical`)\
  If `TRUE` then, in the case of ties, censor times are always
  considered to have occurred after the event times when calculating the
  "reverse Kaplan-Meier" for the IPCW estimates. Setting this to `TRUE`
  mirrors the implementation of the `{prodlim}` package.

- event_offset:

  (`logical`)\
  If `TRUE` then \\G(T_i)\\ is evaluated at \\G(T_i-)\\. Setting this as
  `TRUE` mirrors the implementation of the `{pec}` package.

- ...:

  not used.

## References

Blanche P, Proust-Lima C, Loubère L, Berr C, Dartigues J, Jacqmin-Gadda
H (2015). “Quantifying and comparing dynamic predictive accuracy of
joint models for longitudinal marker and time-to-event in presence of
censoring and competing risks.” *Biometrics*, **71**(1), 102-113.
[doi:10.1111/biom.12232](https://doi.org/10.1111/biom.12232) ,
https://onlinelibrary.wiley.com/doi/pdf/10.1111/biom.12232,
<https://onlinelibrary.wiley.com/doi/abs/10.1111/biom.12232>.

## See also

Other brierScore:
[`brierScore()`](https://genentech.github.io/jmpost/reference/brierScore.md)

Other SurvivalQuantities:
[`SurvivalQuantities-class`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md),
[`as.data.frame.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/as.data.frame.SurvivalQuantities.md),
[`autoplot.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/autoplot.SurvivalQuantities.md),
[`summary.SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/summary.SurvivalQuantities.md)
