# Stan Model Object and Constructor Function

Stan Model Object and Constructor Function

## Usage

``` r
StanModel(stan, parameters, name = "<Unnamed>")
```

## Arguments

- stan:

  (`StanModule`) code containing the Stan code specification.

- parameters:

  (`ParameterList`) the parameter specification.

- name:

  (`character`) display name for the model object.

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
