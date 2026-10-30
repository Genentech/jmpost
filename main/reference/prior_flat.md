# Flat Prior

Specifies a constant density on the declared model parameter scale, for
a scalar or each component of a vector. No prior density term or
auxiliary parameters are added to the Stan model. Model-imposed
parameter bounds still apply. An unbounded flat prior is improper; a
proper posterior is not guaranteed.

## Usage

``` r
prior_flat(init = prior_normal(0, 1))
```

## Arguments

- init:

  (`Prior`) distribution used only to generate initial values, including
  the usual `jmpost.prior_shrinkage` towards its centre. Its density and
  limits do not constrain the model parameter. Choose initial values
  compatible with any model-imposed bounds.

## Value

A `Prior` object.

## See also

[`prior_init_only()`](https://genentech.github.io/jmpost/reference/prior_init_only.md)

Other Prior:
[`prior_beta()`](https://genentech.github.io/jmpost/reference/prior_beta.md),
[`prior_cauchy()`](https://genentech.github.io/jmpost/reference/prior_cauchy.md),
[`prior_const()`](https://genentech.github.io/jmpost/reference/prior_const.md),
[`prior_const_vector()`](https://genentech.github.io/jmpost/reference/prior_const_vector.md),
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
[`prior_transform()`](https://genentech.github.io/jmpost/reference/prior_transform.md),
[`prior_uniform()`](https://genentech.github.io/jmpost/reference/prior_uniform.md)

## Examples

``` r
prior_flat()
#> 
#> Prior Object:
#>    flat()
#> 
prior_flat(init = prior_normal(0, 2))
#> 
#> Prior Object:
#>    flat()
#> 
SurvivalWeibullPH(beta = prior_flat())
#> 
#> Weibull-PH Survival Model with parameters:
#>     sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
#>     sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
#>     beta_os_cov ~ flat()
#> 
```
