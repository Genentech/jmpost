# Resolve a Promise

Resolve a Promise

## Usage

``` r
resolvePromise(object, ...)

# Default S3 method
resolvePromise(object, ...)
```

## Arguments

- object:

  (`ANY`) an object to resolve.

- ...:

  (`ANY`) additional arguments.

  If `object` is not a promise will just return itself else will resolve
  the promise and return the promised object.

## Value

The input object, or the value obtained by resolving a promise.

## Examples

``` r
resolvePromise(1)
#> [1] 1
resolvePromise(linkDSLD(), LongitudinalGSF())
#> 
#> LinkComponent with parameter:
#>     link_dsld ~ normal(mu = 0, sigma = 2)
#> 
```
