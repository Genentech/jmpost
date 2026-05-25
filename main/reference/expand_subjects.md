# `expand_subjects`

This function checks and expands a given subjects vector. The input
vector must be unique and contain only values as specified by
`all_subjects`

## Usage

``` r
expand_subjects(subjects, all_subjects)
```

## Arguments

- subjects:

  (`character` or `NULL`) Character vector representing the subjects. If
  NULL, it will be set to the value of `all_subjects`.

- all_subjects:

  (`character`) Character vector representing all possible subjects.

## Value

Returns the expanded `subjects` vector.
