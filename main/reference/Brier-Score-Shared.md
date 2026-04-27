# Re-used documentation for Brier Score components

Re-used documentation for Brier Score components

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

- event_offset:

  (`logical`)\
  If `TRUE` then \\G(T_i)\\ is evaluated at \\G(T_i-)\\. Setting this as
  `TRUE` mirrors the implementation of the `{pec}` package.

- maintain_cen_order:

  (`logical`)\
  If `TRUE` then, in the case of ties, censor times are always
  considered to have occurred after the event times when calculating the
  "reverse Kaplan-Meier" for the IPCW estimates. Setting this to `TRUE`
  mirrors the implementation of the `{prodlim}` package.

- ...:

  not used.
