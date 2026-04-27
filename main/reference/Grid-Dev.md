# Grid Developer Notes

Developer details for implementing / extending `Grid` objects for
defining generated quantities for
[`SurvivalQuantities`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md)
and
[`LongitudinalQuantities`](https://genentech.github.io/jmpost/reference/LongitudinalQuantities-class.md).

## Details

All grid classes must inherit from the abstract `Grid` class. All grid
classes must provide `as.QuantityGenerator(object, data)` and
`as.QuantityCollapser(object, data)` methods where `data` is a
[`DataJoint`](https://genentech.github.io/jmpost/reference/DataJoint-class.md)
object. These methods must return a `QuantityGenerator` and
`QuantityCollapser` object respectively. The `QuantityGenerator` object
specifies unique subject/timepoint combinations that samples should be
generated at. The `QuantityCollapser` object specifies how to combine
these generated samples to form the desired quantities. As an example
say we want to generate grouped samples for the groups `Group-1` and
`Group-2` which consist of the subjects `sub-1`, `sub-2` and `sub-3`,
`sub-4` respectively at two time points `10` and `20`. We can achieve
this as follows:

    QuantityGenerator(
        times = c(10, 10, 10, 10, 20, 20, 20, 20),
        subjects = c("sub-1" "sub-2", "sub-3", "sub-4", "sub-1" "sub-2", "sub-3", "sub-4")
    )
    QuantityCollapser(
        times = c(10, 20, 10 , 20),
        groups = c("Group-1", "Group-1", "Group-2", "Group-2"),
        indexes = list(c(1, 2), c(5, 6), c(3, 4), c(7, 8))
    )

For population based quantities use the `arms` and `studies` arguments
of `QuantityGenerator` instead of `subjects`.

## Slots

- `subjects`:

  (`character` or `NULL`)\
  vector of subjects to extract quantities for. If `NULL` will default
  to all subjects within the dataset.

- `times`:

  (`numeric` or `NULL`)\
  vector of time points to extract quantities at. If `NULL` will default
  to 201 evenly spaced timepoints between 0 and either the max

- `groups`:

  (`list`)\
  named list of subjects to extract quantities for. See details.

## Group Specification

For
[`GridGrouped()`](https://genentech.github.io/jmpost/reference/Grid-Functions.md),
`groups` must be a named list of character vectors. Each element of the
list must be a character vector of the subjects that will form the group
where the element name is the corresponding name of the group. For
example if the goal was to create two groups named `Group-1` and
`Group-2` which are composed of the subjects `sub-1`, `sub-2` and
`sub-3`, `sub-4` respectively then this would be specified as:

    GridGrouped(
        groups = list(
            "Group-1" = c("sub-1", "sub-2"),
            "Group-2" = c("sub-3", "sub-4")
        )
    )

## See also

`Quant-Dev`
