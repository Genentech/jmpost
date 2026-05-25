# Survival Plot

Internal plotting function to create survival plots with KM curve
overlays This function predominately exists to extract core logic into
its own function to enable easier unit testing.

## Usage

``` r
survival_plot(
  data,
  add_ci = TRUE,
  add_wrap = TRUE,
  kmdf = NULL,
  y_label = expression(S(t)),
  x_label = expression(t)
)
```

## Arguments

- data:

  (`data.frame`) summary statistics for a survival curve to be plotted.
  See details.

- add_ci:

  (`logical`) should confidence intervals be added? Default = `TRUE`.

- add_wrap:

  (`logical`) should the plots be wrapped by `data$group`? Default =
  `TRUE`.

- kmdf:

  (`data.frame` or `NULL`) event times and status used to plot
  overlaying KM curves. If `NULL` no KM curve will be plotted. See
  details.

- y_label:

  (`character` or `expression`) label to display on the y-axis. Default
  = `expression(S(t))`.

- x_label:

  (`character` or `expression`) label to display on the x-axis.

## Details

### `data`

Should contain the following columns:

- `time` (`numeric`)\
  time point for the summary statistic.

- `group` (`character`)\
  the group in which the observation belongs to.

- `median` (`numeric`)\
  the median value for the summary statistic.

- `upper` (`numeric`)\
  the upper 95% CI for the summary statistic.

- `lower` (`numeric`)\
  the lower 95% CI for the summary statistic.

### `kmdf`

Should contain the following columns:

- `time` (`numeric`)\
  the time at which an event occurred.

- `event` (`numeric`)\
  1/0 status indicator for the event.

- `group` (`character`)\
  which group the event belongs to, should correspond to values in
  `data$group`.
