# Enable Link Generic

Enable Link Generic

## Usage

``` r
enableLink(object, ...)
```

## Arguments

- object:

  (`LongitudinalModel`) to enable link for.

- ...:

  Not used.

  Optional hook method that is called on a
  [`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  only if a link method is provided to
  [`JointModel`](https://genentech.github.io/jmpost/reference/JointModel-class.md).
  This can be used to allow the model to include any optional stan code
  that is only required if there are links present.

## Value

[`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
object

## Examples

``` r
enableLink(LongitudinalGSF())
#> 
#> Generalized Stein-Fojo Longitudinal Model (additive error) with parameters:
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
