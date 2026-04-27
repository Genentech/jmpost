# Brier Score

Implements the Brier Score as detailed in (Blanche et al. 2015)

## Usage

``` r
brier_score(
  t,
  times,
  events,
  pred_mat,
  maintain_cen_order = TRUE,
  event_offset = TRUE
)

bs_get_squared_dist(t, times, events, pred_mat)

bs_get_weights(
  t,
  times,
  events,
  event_offset = TRUE,
  maintain_cen_order = TRUE
)
```

## Arguments

- t:

  (`numeric`)\
  timepoints to calculate the desired quantity at.

- times:

  (`numeric`)\
  observed times.

- events:

  (`numeric`)\
  event indicator for `times`. Either 1 for an event or 0 for censor.

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

## Details

- `bs_get_squared_dist()` - implements the squared distance part of the
  formula.

- `bs_get_weights()` - implements the IPCW weighting

## References

Blanche P, Proust-Lima C, Loubère L, Berr C, Dartigues J, Jacqmin-Gadda
H (2015). “Quantifying and comparing dynamic predictive accuracy of
joint models for longitudinal marker and time-to-event in presence of
censoring and competing risks.” *Biometrics*, **71**(1), 102-113.
[doi:10.1111/biom.12232](https://doi.org/10.1111/biom.12232) ,
https://onlinelibrary.wiley.com/doi/pdf/10.1111/biom.12232,
<https://onlinelibrary.wiley.com/doi/abs/10.1111/biom.12232>.
