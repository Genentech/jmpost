# Print method for LongitudinalSteinFojo works as expected

    Code
      print(x)
    Output
      
      Stein-Fojo Longitudinal Model (additive error) with parameters:
          lm_sf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
          lm_sf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
          lm_sf_mu_kg ~ normal(mu = -1.20397, sigma = 1)
          lm_sf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_sigma ~ lognormal(mu = -2.30259, sigma = 1)
          lm_sf_log_psi_bsld ~ <Hierarchical normal on log scale>
          lm_sf_log_psi_ks ~ <Hierarchical normal on log scale>
          lm_sf_log_psi_kg ~ <Hierarchical normal on log scale>
      

---

    Code
      print(x)
    Output
      
      Stein-Fojo Longitudinal Model (additive error) with parameters:
          lm_sf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
          lm_sf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
          lm_sf_mu_kg ~ gamma(alpha = 2, beta = 1)
          lm_sf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_sigma ~ normal(mu = 0, sigma = 1) T[0, ]
          lm_sf_log_psi_bsld ~ <Hierarchical normal on log scale>
          lm_sf_log_psi_ks ~ <Hierarchical normal on log scale>
          lm_sf_log_psi_kg ~ <Hierarchical normal on log scale>
      

