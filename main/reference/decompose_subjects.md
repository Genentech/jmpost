# Decompose subjects into Relevant Components

This function takes in a character vector or list of subjects and
decomposes it into a structured format.

## Usage

``` r
decompose_subjects(subjects, all_subjects)
```

## Arguments

- subjects:

  (`character` or `list`)\
  subject identifiers. If `NULL` will be set to `all_subjects`.

- all_subjects:

  (`character`)\
  the set of allowable subject identifiers. Will cause an error if any
  value of `subjects` is not in this vector.

## Value

A list containing three components:

- `groups`: (`list`)\
  each element of the list is a character vector specifying which
  subjects belong to a given "group" where the "group" is the element
  name

- `unique_values`: (`character`)\
  vector of the unique subjects within `subjects`

- `indexes`: (`list`)\
  each element is a named and is a numeric index vector that maps the
  values of `grouped` to `unique_values`

## Details

The primary use of this function is to correctly setup indexing
variables for predicting survival quantities (see
[`SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md))

## See also

[`expand_subjects()`](https://genentech.github.io/jmpost/reference/expand_subjects.md),
[`SurvivalQuantities()`](https://genentech.github.io/jmpost/reference/SurvivalQuantities-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- decompose_subjects(c("A", "B"), c("A", "B", "C", "D"))
result <- decompose_subjects(
    list("g1" = c("A", "B"), "g2" = c("B", "C")),
    c("A", "B", "C", "D")
)
} # }
```
