# Expand and Validate Subjects

If `subjects` is `NULL` this will return a named list of all subjects in
`data`. Else it will return `subjects` as a named list ensuring that all
subjects exist in `data`.

## Usage

``` r
subjects_to_list(subjects = NULL, data)
```

## Arguments

- subjects:

  (`character`) vector of subjects that should exist in `data`

- data:

  (`DataJoint`) Survival and Longitudinal Data.
