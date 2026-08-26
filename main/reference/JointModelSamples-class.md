# `JointModelSamples`

Contains samples from a
[`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).

## Value

A `JointModelSamples` object.

## Slots

- `model`:

  ([`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md))\
  the model that the samples were drawn from.

- `data`:

  ([`DataJoint`](https://genentech.github.io/jmpost/reference/DataJoint-class.md))\
  the data that the model was fitted on.

- `results`:

  ([`cmdstanr::CmdStanMCMC`](https://rdrr.io/pkg/cmdstanr/man/CmdStanMCMC.html))\
  the STAN samples.

## Examples

``` r
if (FALSE) { # \dontrun{
# JointModelSamples objects are returned by sampleStanModel().
fit <- sampleStanModel(JointModel(LongitudinalGSF()), data = joint_data)
} # }
```
