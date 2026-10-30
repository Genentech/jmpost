# Transformations to the Prior Density Scale

`prior_transform_exp()` describes `exp(scale * x)` for an unconstrained
model parameter `x`. `prior_transform_square()` describes `x^2` on the
non-negative branch only. Both map to a non-negative density scale, with
strictly positive values in the interior of the parameter domain.

## Usage

``` r
prior_transform_exp(scale = 1)

prior_transform_square()
```

## Arguments

- scale:

  (`number`) finite positive multiplier inside the exponential. Use `2`
  for a prior on the variance when `x` is a log standard deviation.

## Value

A `PriorTransform` object.

## Details

Supply these objects as the `transform` argument to a scalar
distribution helper such as
[`prior_normal()`](https://genentech.github.io/jmpost/reference/prior_normal.md)
or to
[`prior_transform()`](https://genentech.github.io/jmpost/reference/prior_transform.md).

The intended density is `p(g(x)) * abs(g'(x))`, including the Jacobian.
The square transformation requires a non-negative model parameter so
that it is one-to-one. It must not silently fold an unconstrained
parameter.

## Examples

``` r
prior_transform_exp()
#> An object of class "PriorTransform"
#> Slot "kind":
#> [1] "exp"
#> 
#> Slot "scale":
#> [1] 1
#> 
prior_transform_exp(scale = 2)
#> An object of class "PriorTransform"
#> Slot "kind":
#> [1] "exp"
#> 
#> Slot "scale":
#> [1] 2
#> 
prior_transform_square()
#> An object of class "PriorTransform"
#> Slot "kind":
#> [1] "square"
#> 
#> Slot "scale":
#> [1] 1
#> 
prior_normal(60, 60, transform = prior_transform_exp())
#> 
#> Prior Object:
#>    normal(mu = 60, sigma = 60) T[0, ] on exp(x)
#> 
```
