# `Prior` Function Arguments

The documentation lists all the conventional arguments for
[`Prior`](https://genentech.github.io/jmpost/reference/Prior-class.md)
constructors.

## Arguments

- centre:

  (`number`) the central point of distribution to shrink sampled values
  towards (for most distributions this is the mean or median if the mean
  is undefined)

- x:

  (`Prior`) a prior Distribution

- object:

  (`Prior`) a prior Distribution

- name:

  (`character`) the name of the parameter the prior distribution is for

- size:

  (`numeric` or `character`) the parameter size.

- transform:

  (`PriorTransform | NULL`) optional package-defined transformation from
  the model parameter to the prior density scale. See
  [`prior_transform()`](https://genentech.github.io/jmpost/reference/prior_transform.md)
  for support, Jacobians and examples. `NULL` keeps the prior on the
  model parameter scale.

- ...:

  Not Used.
