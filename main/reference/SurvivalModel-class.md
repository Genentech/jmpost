# `SurvivalModel`

This class extends the general
[`StanModel`](https://genentech.github.io/jmpost/reference/StanModel-class.md)
class to comprise the survival model specification.

## Usage

``` r
SurvivalModel(
  stan = StanModule(),
  parameters = ParameterList(),
  name = "<Unnamed>",
  ...
)
```

## Arguments

- stan:

  (`StanModule`) code containing the model-specific Stan code
  specification.

- parameters:

  (`ParameterList`) the parameter declaration and prior specification.

- name:

  (`character`) display name for the model object.

- ...:

  additional arguments for
  [`StanModel()`](https://genentech.github.io/jmpost/reference/StanModel-class.md).

## Value

A `SurvivalModel` object.

## Examples

``` r
SurvivalModel()
#> 
#> <Unnamed> Survival Model with parameters:
#>     <No Parameters>
#> 
```
