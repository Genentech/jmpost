# Print method for LongitudinalSteinFojo works as expected

    Code
      x <- LongitudinalSteinFojo()
      print(x)
    Output
      
      Stein-Fojo Longitudinal Model with parameters:
          lm_sf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
          lm_sf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
          lm_sf_mu_kg ~ normal(mu = -1.20397, sigma = 1)
          lm_sf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_sigma ~ lognormal(mu = -2.30259, sigma = 1)
          lm_sf_eta_tilde_bsld ~ std_normal()
          lm_sf_eta_tilde_ks ~ std_normal()
          lm_sf_eta_tilde_kg ~ std_normal()
      

---

    Code
      x <- LongitudinalSteinFojo(sigma = prior_normal(0, 1), mu_kg = prior_gamma(2, 1))
      print(x)
    Output
      
      Stein-Fojo Longitudinal Model with parameters:
          lm_sf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
          lm_sf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
          lm_sf_mu_kg ~ gamma(alpha = 2, beta = 1)
          lm_sf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
          lm_sf_sigma ~ normal(mu = 0, sigma = 1)
          lm_sf_eta_tilde_bsld ~ std_normal()
          lm_sf_eta_tilde_ks ~ std_normal()
          lm_sf_eta_tilde_kg ~ std_normal()
      

