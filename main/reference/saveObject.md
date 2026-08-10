# Save Object to File

Save Object to File

## Usage

``` r
saveObject(object, file, ...)
```

## Arguments

- object:

  (`ANY`) object to save.

- file:

  (`character`) file to save object to.

- ...:

  (`ANY`) additional arguments.

## Value

Invisibly returns `NULL` after saving the object.

## See also

Other saveObject:
[`saveObject.JointModelSamples()`](https://genentech.github.io/jmpost/reference/saveObject.JointModelSamples.md)

## Examples

``` r
if (FALSE) { # \dontrun{
saveObject(fit, "joint-model-fit.rds")
} # }
```
