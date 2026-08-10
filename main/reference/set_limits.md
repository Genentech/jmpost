# Set Constraints

Applies constraints to a prior distribution to ensure any sampled
numbers from the distribution fall within the constraints

## Usage

``` r
set_limits(object, lower = -Inf, upper = Inf)

# S3 method for class 'Prior'
set_limits(object, lower = -Inf, upper = Inf)
```

## Arguments

- object:

  (`Prior`) a prior distribution to apply constraints to

- lower:

  (`numeric`) lower constraint boundary

- upper:

  (`numeric`) upper constraint boundary

## Value

A `Prior` object with the requested bounds.

## Examples

``` r
set_limits(prior_normal(0, 1), lower = 0)
#> 
#> Prior Object:
#>    normal(mu = 0, sigma = 1) T[0, ]
#> 
```
