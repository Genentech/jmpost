# Joint Model Object and Constructor Function

Joint Model Object and Constructor Function

## Usage

``` r
JointModel(longitudinal = NULL, survival = NULL, link = Link())
```

## Arguments

- longitudinal:

  (`LongitudinalModel` or `NULL`) the longitudinal model.

- survival:

  (`SurvivalModel` or `NULL`) the survival model.

- link:

  (`Link`) the link.

## Value

A `JointModel` object.

## Slots

- `longitudinal`:

  ([`LongitudinalModel`](https://genentech.github.io/jmpost/reference/LongitudinalModel-class.md)
  or `NULL`)\
  the longitudinal model.

- `survival`:

  ([`SurvivalModel`](https://genentech.github.io/jmpost/reference/SurvivalModel-class.md)
  or `NULL`)\
  the survival model.

- `link`:

  (`Link`)\
  the link.

- `parameters`:

  (`ParameterList`)\
  the parameter declaration and prior specification.

## See also

Other JointModel:
[`as.StanModule.JointModel()`](https://genentech.github.io/jmpost/reference/as.StanModule.JointModel.md),
[`as.character.JointModel()`](https://genentech.github.io/jmpost/reference/as.character.JointModel.md)

## Examples

``` r
JointModel(LongitudinalGSF(), SurvivalWeibullPH(), linkDSLD())
#> 
#> A Joint Model with:
#> 
#>   Survival:     
#>      Weibull-PH Survival Model with parameters:
#>          sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
#>          sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
#>          beta_os_cov ~ normal(mu = 0, sigma = 2)
#>      
#>   Longitudinal:     
#>      Generalized Stein-Fojo Longitudinal Model (additive error) with parameters:
#>          lm_gsf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
#>          lm_gsf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
#>          lm_gsf_mu_kg ~ normal(mu = -1.20397, sigma = 1)
#>          lm_gsf_mu_phi ~ normal(mu = 0, sigma = 1)
#>          lm_gsf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
#>          lm_gsf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
#>          lm_gsf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
#>          lm_gsf_omega_phi ~ lognormal(mu = -1.60944, sigma = 1)
#>          lm_gsf_sigma ~ lognormal(mu = -2.30259, sigma = 1)
#>          lm_gsf_eta_tilde_bsld ~ std_normal()
#>          lm_gsf_eta_tilde_ks ~ std_normal()
#>          lm_gsf_eta_tilde_kg ~ std_normal()
#>          lm_gsf_eta_tilde_phi ~ std_normal()
#>      
#>   Link:     
#>      Link with the following components/parameters:
#>          link_dsld ~ normal(mu = 0, sigma = 2)
```
