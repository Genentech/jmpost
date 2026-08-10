# Stan Model Object and Constructor Function

Stan Model Object and Constructor Function

## Usage

``` r
StanModel(stan, parameters, name = "<Unnamed>")
```

## Arguments

- stan:

  (`StanModule`) code containing the model-specific Stan code
  specification.

- parameters:

  (`ParameterList`) the parameter declaration and prior specification.

- name:

  (`character`) display name for the model object.

## Value

A `StanModel` object.

## Slots

- `stan`:

  (`StanModule`)\
  See Arguments.

- `parameters`:

  (`ParameterList`)\
  See Arguments.

- `name`:

  (`character`)\
  display name for the model object.

## See also

Other StanModel:
[`as.list.StanModel()`](https://genentech.github.io/jmpost/reference/as.list.StanModel.md)

## Examples

``` r
StanModel(StanModule(), ParameterList(), name = "Example model")
#> 
#> Example model Model Object with parameters:
#>     <No Parameters>
#> 
```
