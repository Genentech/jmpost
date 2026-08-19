# Regularized Horseshoe Prior for a Vector Distribution

Regularized Horseshoe Prior for a Vector Distribution

## Usage

``` r
prior_horseshoe(
  df = 1,
  df_global = 1,
  df_slab = 4,
  scale_global = 1,
  scale_slab = 2
)
```

## Arguments

- df:

  (`number`) degrees of freedom of the half-Student-t prior for local
  shrinkage parameters.

- df_global:

  (`number`) degrees of freedom of the half-Student-t prior for the
  global shrinkage parameter.

- df_slab:

  (`number`) degrees of freedom of the Student-t slab.

- scale_global:

  (`number`) scale of the half-Student-t prior for the global shrinkage
  parameter.

- scale_slab:

  (`number`) scale of the Student-t slab.

## Value

A `Prior` object.

## See also

Other Prior:
[`prior_beta()`](https://genentech.github.io/jmpost/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/jmpost/reference/prior_cauchy.md),
[`prior_const()`](https://genentech.github.io/jmpost/reference/prior_const.md),
[`prior_const_vector()`](https://genentech.github.io/jmpost/reference/prior_const_vector.md),
[`prior_gamma()`](https://genentech.github.io/jmpost/reference/prior_gamma.md),
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
prior_horseshoe(df = 1, df_global = 1, df_slab = 4, scale_global = 0.1, scale_slab = 2)
#> 
#> Prior Object:
#>    horseshoe(df = 1, df_global = 1, df_slab = 4, scale_global = 0.1, scale_slab = 2)
#> 
```
