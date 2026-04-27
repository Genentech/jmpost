# Subsetting `DataJoint` as a `data.frame`

Coerces the object into a `data.frame` containing just event times and
status filtering for specific subjects If `subjects` is a list then an
additional variable `group` will be added onto the dataset specifying
which group the row belongs to.

## Usage

``` r
# S3 method for class 'DataJoint'
subset(x, subjects, ...)
```

## Arguments

- x:

  (`DataJoint`)\
  object created by
  [`DataJoint()`](https://genentech.github.io/jmpost/reference/DataJoint-class.md).

- subjects:

  (`character` or `list`)\
  subjects that you wish to subset the `data.frame` to contain. See
  details.

- ...:

  Not used.

## See also

Other DataJoint:
[`DataJoint-class`](https://genentech.github.io/jmpost/reference/DataJoint-class.md),
[`as_stan_list.DataSubject()`](https://genentech.github.io/jmpost/reference/as_stan_list.DataObject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
subjects <- c("SUB1", "SUB2", "SUB3", "SUB4")
subset(x, subjects)

groups <- list(
    "g1" = c("SUB1", "SUB3", "SUB4"),
    "g2" = c("SUB2", "SUB3")
)
subset(x, groups)
} # }
```
