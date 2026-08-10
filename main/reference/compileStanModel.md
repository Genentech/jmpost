# `compileStanModel`

Compile the Stan module.

## Usage

``` r
compileStanModel(object)

# S3 method for class 'StanModule'
compileStanModel(object)

# S3 method for class 'JointModel'
compileStanModel(object)
```

## Arguments

- object:

  the module.

## Value

A compiled
[`cmdstanr::CmdStanModel`](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
compileStanModel(JointModel(LongitudinalGSF()))
} # }
```
