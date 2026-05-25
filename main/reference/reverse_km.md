# Reverse Kaplan-Meier

Calculates the survival estimates of the censoring distribution using
the Kaplan-Meier estimate. This is primarily used in the calculation of
the IPCW estimates.

## Usage

``` r
reverse_km_event_first(t, times, events)

reverse_km_cen_first(t, times, events)
```

## Arguments

- t:

  (`numeric`) timepoints to calculate the desired quantity at.

- times:

  (`numeric`) observed times.

- events:

  (`numeric`) event indicator for `times`. Either 1 for an event or 0
  for censor.

## Details

With regards to ties between censor and event times; the standard
approach is to regard events as occurring before censors. However, when
modelling the censoring distribution we are regarding the censors as
"events" so which should come first in the case of ties?

The `reverse_km_event_first()` function maintains the rule that events
always come first even if we are regarding the censors as "events". This
matches the implementation of `prodlim::prodlim(..., reverse = TRUE)`.

The `reverse_km_cen_first()` function provides the alternative
implementation assuming that in the case of ties the censor "events"
come before the event "censors". This is essentially a thin wrapper
around `survival::survfit(Surv(time, 1 - event), ...)`
