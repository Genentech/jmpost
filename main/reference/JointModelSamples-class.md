# `JointModelSamples`

Contains samples from a
[`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).

## Slots

- `model`:

  ([`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md))\
  the model that the samples were drawn from.

- `data`:

  ([`DataJoint`](https://genentech.github.io/jmpost/reference/DataJoint-class.md))\
  the data that the model was fitted on.

- `results`:

  ([`cmdstanr::CmdStanMCMC`](https://mc-stan.org/cmdstanr/reference/CmdStanMCMC.html))\
  the STAN samples.
