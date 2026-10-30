# Prior on a Transformed Parameter

Places a distribution on `g(x)` rather than directly on the declared
model parameter `x`, where `g` is a package-defined transformation. The
Stan density includes the log absolute Jacobian of `g`. The same scalar
distribution is applied independently to each component of a vector.

## Usage

``` r
prior_transform(dist, transform)
```

## Arguments

- dist:

  (`Prior`) a scalar distribution prior: normal, standard normal,
  Cauchy, gamma, log-normal, beta, uniform, Student-t, logistic,
  log-logistic or inverse-gamma. Constant, flat, initialization-only,
  vector-specific, horseshoe and already transformed priors are not
  supported.

- transform:

  (`PriorTransform`) package-defined transformation from the model
  parameter to the prior density scale, created by
  [`prior_transform_exp()`](https://genentech.github.io/jmpost/reference/prior-transformations.md)
  or
  [`prior_transform_square()`](https://genentech.github.io/jmpost/reference/prior-transformations.md).

## Value

A `TransformedPrior` object, inheriting from `Prior`.

## Details

Equivalently, supply `transform` directly to a scalar distribution
helper. The density is proportional to `p(g(x)) * abs(g'(x))`.
Distribution support and any existing limits on `dist` are intersected
with the transformation's range and mapped back to bounds on `x`.
Subsequent
[`set_limits()`](https://genentech.github.io/jmpost/reference/set_limits.md)
calls use the model parameter scale and retain these support
restrictions. Truncation normalisation is evaluated on the density
scale.

Initial values are drawn on the density scale within the mapped bounds,
inverse-transformed and shrunk towards a centre on the model parameter
scale using `jmpost.prior_shrinkage`. If the inverse-transformed
distribution centre is outside the bounds or non-finite, an interior
reference value is used. Initialization uses bounded rejection sampling
and reports an error when the admissible region has too little
probability to find valid draws.

This changes the prior distribution. Predictor parametrizations and Stan
affine multipliers serve different purposes and do not supply this
Jacobian.

## Slots

- `distribution`:

  (`Prior`)\
  Distribution on the transformed scale.

- `transform`:

  (`PriorTransform`)\
  Transformation to the density scale.

## See also

Other Prior:
[`prior_beta()`](https://genentech.github.io/jmpost/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/jmpost/reference/prior_cauchy.md),
[`prior_const()`](https://genentech.github.io/jmpost/reference/prior_const.md),
[`prior_const_vector()`](https://genentech.github.io/jmpost/reference/prior_const_vector.md),
[`prior_flat()`](https://genentech.github.io/jmpost/reference/prior_flat.md),
[`prior_gamma()`](https://genentech.github.io/jmpost/reference/prior_gamma.md),
[`prior_horseshoe()`](https://genentech.github.io/jmpost/reference/prior_horseshoe.md),
[`prior_init_only()`](https://genentech.github.io/jmpost/reference/prior_init_only.md),
[`prior_invgamma()`](https://genentech.github.io/jmpost/reference/prior_invgamma.md),
[`prior_logistic()`](https://genentech.github.io/jmpost/reference/prior_logistic.md),
[`prior_loglogistic()`](https://genentech.github.io/jmpost/reference/prior_loglogistic.md),
[`prior_lognormal()`](https://genentech.github.io/jmpost/reference/prior_lognormal.md),
[`prior_normal()`](https://genentech.github.io/jmpost/reference/prior_normal.md),
[`prior_normal_vector()`](https://genentech.github.io/jmpost/reference/prior_normal_vector.md),
[`prior_std_normal()`](https://genentech.github.io/jmpost/reference/prior_std_normal.md),
[`prior_student_t()`](https://genentech.github.io/jmpost/reference/prior_student_t.md),
[`prior_uniform()`](https://genentech.github.io/jmpost/reference/prior_uniform.md)

## Examples

``` r
prior_normal(60, 60, transform = prior_transform_exp())
#> 
#> Prior Object:
#>    normal(mu = 60, sigma = 60) T[0, ] on exp(x)
#> 
prior_invgamma(1.5, 0.3, transform = prior_transform_exp(scale = 2))
#> 
#> Prior Object:
#>    inv_gamma(alpha = 1.5, beta = 0.3) on exp(2 * (x))
#> 
prior_transform(prior_normal(100, 100), prior_transform_square())
#> 
#> Prior Object:
#>    normal(mu = 100, sigma = 100) T[0, ] on square(x); x T[0, ]
#> 
```
