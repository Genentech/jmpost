# Extract Observed Longitudinal Values

Utility function to extract the observed longitudinal values from a
[`DataJoint`](https://genentech.github.io/jmpost/reference/DataJoint-class.md)
object

## Usage

``` r
extract_observed_values(object)
```

## Arguments

- object:

  (`DataJoint`) data used to fit a
  [`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).

## Value

A data.frame with the following columns

- `subject` (`character`)\
  The subject identifier

- `time` (`numeric`)\
  The time at which the observation occurred

- `Yob` (`numeric`)\
  The observed value
