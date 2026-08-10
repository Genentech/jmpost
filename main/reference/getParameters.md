# `getParameters`

Extract any modelling parameters as a
[`ParameterList`](https://genentech.github.io/jmpost/reference/ParameterList-class.md)
object from a model.

## Usage

``` r
getParameters(object, ...)

# S3 method for class 'StanModel'
getParameters(object, ...)

# S3 method for class 'LinkComponent'
getParameters(object, ...)

# S3 method for class 'Link'
getParameters(object, ...)

# Default S3 method
getParameters(object, ...)
```

## Arguments

- object:

  where to obtain the parameters from.

- ...:

  additional options.

## Value

A `ParameterList` object.

## See also

Other LinkComponent:
[`LinkComponent-class`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md),
[`as.StanModule.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.StanModule.LinkComponent.md),
[`as.list.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.list.LinkComponent.md),
[`initialValues()`](https://genentech.github.io/jmpost/reference/initialValues.md)

## Examples

``` r
getParameters(LongitudinalGSF())
#> 
#> ParameterList Object:
#>     lm_gsf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
#>     lm_gsf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
#>     lm_gsf_mu_kg ~ normal(mu = -1.20397, sigma = 1)
#>     lm_gsf_mu_phi ~ normal(mu = 0, sigma = 1)
#>     lm_gsf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
#>     lm_gsf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
#>     lm_gsf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
#>     lm_gsf_omega_phi ~ lognormal(mu = -1.60944, sigma = 1)
#>     lm_gsf_sigma ~ lognormal(mu = -2.30259, sigma = 1)
#>     lm_gsf_eta_tilde_bsld ~ std_normal()
#>     lm_gsf_eta_tilde_ks ~ std_normal()
#>     lm_gsf_eta_tilde_kg ~ std_normal()
#>     lm_gsf_eta_tilde_phi ~ std_normal()
#> 
```
