# Longitudinal Plot

Internal plotting function to create longitudinal plots This function
predominately exists to extract core logic into its own function to
enable easier unit testing.

## Usage

``` r
longitudinal_plot(data, data_obs = NULL, add_ci = FALSE)
```

## Arguments

- data:

  (`data.frame`) summary statistics for longitudinal value estimates.
  See details.

- data_obs:

  (`data.frame`) real observed values to be overlaid for reference. See
  details.

- add_ci:

  (`logical`) Should confidence intervals be added? Default = `TRUE`.

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

### `data_obs`

Should contain the following columns:

- `time` (`numeric`)\
  the time at which the observed value occurred.

- `Yob` (`numeric`)\
  the real observed value.

- `group` (`character`)\
  which group the event belongs to, should correspond to values in
  `data$group`.
