# Joint Model Object and Constructor Function

Joint Model Object and Constructor Function

## Usage

``` r
JointModel(longitudinal = NULL, survival = NULL, link = Link())
```

## Arguments

- longitudinal:

  ([`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  or `NULL`)\
  the longitudinal model.

- survival:

  ([`SurvivalModel`](https://genentech.github.io/jmpost/reference/SurvivalModel-class.md)
  or `NULL`)\
  the survival model.

- link:

  (`Link`)\
  the link.

## Slots

- `longitudinal`:

  ([`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  or `NULL`)\
  the longitudinal model.

- `survival`:

  ([`SurvivalModel`](https://genentech.github.io/jmpost/reference/SurvivalModel-class.md)
  or `NULL`)\
  the survival model.

- `link`:

  (`Link`)\
  the link.

- `parameters`:

  (`ParameterList`)\
  the parameter specification.

## See also

Other JointModel:
[`as.StanModule.JointModel()`](https://genentech.github.io/jmpost/reference/as.StanModule.JointModel.md),
[`as.character.JointModel()`](https://genentech.github.io/jmpost/reference/as.character.JointModel.md)
