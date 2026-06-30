# Prior Object and Constructor Function

Specifies the prior distribution in a Stan Model

## Usage

``` r
Prior(
  parameters,
  display,
  repr_model,
  repr_data,
  centre,
  validation,
  sample,
  limits = c(-Inf, Inf),
  .allow_vectors = FALSE
)
```

## Arguments

- parameters:

  (`list`) the prior distribution parameters.

- display:

  (`string`) the string to display when object is printed.

- repr_model:

  (`string`) the Stan code representation for the model block.

- repr_data:

  (`string`) the Stan code representation for the data block.

- centre:

  (`numeric`) the central point of distribution to shrink sampled values
  towards

- validation:

  (`list`) the prior distribution parameter validation functions. Must
  have the same names as the `paramaters` slot.

- sample:

  (`function`) a function to sample from the prior distribution.

- limits:

  (`numeric`) the lower and upper limits for a truncated distribution

- .allow_vectors:

  (`flag`) whether to allow vector parameters.

## Slots

- `parameters`:

  (`list`)\
  See arguments.

- `repr_model`:

  (`string`)\
  See arguments.

- `repr_data`:

  (`string`)\
  See arguments.

- `centre`:

  (`numeric`)\
  See arguments.

- `validation`:

  (`list`)\
  See arguments.

- `display`:

  (`string`)\
  See arguments.

- `sample`:

  (`function`)\
  See arguments.

- `limits`:

  (`numeric`)\
  See arguments.

- `.allow_vectors`:

  (`logical`)\
  See arguments.

## See also

Other Prior-internal:
[`Prior-Getter-Methods`](https://genentech.github.io/jmpost/reference/Prior-Getter-Methods.md),
[`as.StanModule.Prior()`](https://genentech.github.io/jmpost/reference/as.StanModule.Prior.md),
[`as.character.Prior()`](https://genentech.github.io/jmpost/reference/as.character.Prior.md),
[`as_stan_list.Prior()`](https://genentech.github.io/jmpost/reference/as_stan_list.Prior.md)
