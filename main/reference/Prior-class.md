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
  repr_parameters = "",
  repr_transformed_parameters = "",
  repr_generated_quantities = "",
  auxiliary_initial_values = function(name, size) list(),
  auxiliary_size = function(name, size) list(),
  limits = c(-Inf, Inf),
  .omit_zero_lower_truncation = FALSE,
  .allow_vectors = FALSE,
  .is_const = FALSE
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

- repr_parameters:

  (`string`) the Stan code representation for the parameters block.

- repr_transformed_parameters:

  (`string`) the Stan code representation for the transformed parameters
  block.

- repr_generated_quantities:

  (`string`) the Stan code representation for the generated quantities
  block.

- auxiliary_initial_values:

  (`function`) a function that returns initial values for extra Stan
  parameters introduced by the prior.

- auxiliary_size:

  (`function`) a function that returns sizes for extra Stan parameters
  introduced by the prior.

- limits:

  (`numeric`) the lower and upper limits for a truncated distribution.

- .omit_zero_lower_truncation:

  (`flag`) whether to omit a lower-zero truncation adjustment.

- .allow_vectors:

  (`flag`) whether to allow vector parameters.

- .is_const:

  (`flag`) whether this prior fixes the parameter at a constant value.

## Value

A `Prior` object.

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

- `repr_parameters`:

  (`string`)\
  See arguments.

- `repr_transformed_parameters`:

  (`string`)\
  See arguments.

- `repr_generated_quantities`:

  (`string`)\
  See arguments.

- `auxiliary_initial_values`:

  (`function`)\
  See arguments.

- `auxiliary_size`:

  (`function`)\
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

- `.omit_zero_lower_truncation`:

  (`logical`)\
  See arguments.

- `.allow_vectors`:

  (`logical`)\
  See arguments.

- `.is_const`:

  (`logical`)\
  See arguments.

## See also

Other Prior-internal:
[`Prior-Getter-Methods`](https://genentech.github.io/jmpost/reference/Prior-Getter-Methods.md),
[`as.StanModule.Prior()`](https://genentech.github.io/jmpost/reference/as.StanModule.Prior.md),
[`as.character.Prior()`](https://genentech.github.io/jmpost/reference/as.character.Prior.md),
[`as_stan_list.Prior()`](https://genentech.github.io/jmpost/reference/as_stan_list.Prior.md)

## Examples

``` r
# Prior objects are normally created with a distribution helper.
prior_normal(0, 1)
#> 
#> Prior Object:
#>    normal(mu = 0, sigma = 1)
#> 
```
