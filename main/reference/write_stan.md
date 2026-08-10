# `write_stan`

Write the Stan code for a Stan module.

## Usage

``` r
write_stan(object, destination, ...)

# S3 method for class 'JointModel'
write_stan(object, destination, ...)
```

## Arguments

- object:

  the module.

- destination:

  (`character` or `connection`) Where to write stan code to.

- ...:

  Additional arguments

## Value

Invisibly returns `NULL` after writing the Stan program.

## Examples

``` r
path <- tempfile(fileext = ".stan")
write_stan(JointModel(LongitudinalGSF()), path)
unlink(path)
```
