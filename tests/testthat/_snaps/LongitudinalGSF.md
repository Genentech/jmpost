# Print method for LongitudinalGSF works as expected

    Code
      x <- LongitudinalGSF()
      print(x)
    Output
      
      Generalized Stein-Fojo Longitudinal Model with parameters:
          lm_gsf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
          lm_gsf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
          lm_gsf_mu_kg ~ normal(mu = -1.20397, sigma = 1)
          lm_gsf_mu_phi ~ normal(mu = 0, sigma = 1)
          lm_gsf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_omega_phi ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_sigma ~ lognormal(mu = -2.30259, sigma = 1)
          lm_gsf_eta_tilde_bsld ~ std_normal()
          lm_gsf_eta_tilde_ks ~ std_normal()
          lm_gsf_eta_tilde_kg ~ std_normal()
          lm_gsf_eta_tilde_phi ~ std_normal()
      

---

    Code
      x <- LongitudinalGSF(sigma = prior_normal(0, 1), mu_kg = prior_gamma(2, 1))
      print(x)
    Output
      
      Generalized Stein-Fojo Longitudinal Model with parameters:
          lm_gsf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
          lm_gsf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
          lm_gsf_mu_kg ~ gamma(alpha = 2, beta = 1)
          lm_gsf_mu_phi ~ normal(mu = 0, sigma = 1)
          lm_gsf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_omega_phi ~ lognormal(mu = -1.60944, sigma = 1)
          lm_gsf_sigma ~ normal(mu = 0, sigma = 1)
          lm_gsf_eta_tilde_bsld ~ std_normal()
          lm_gsf_eta_tilde_ks ~ std_normal()
          lm_gsf_eta_tilde_kg ~ std_normal()
          lm_gsf_eta_tilde_phi ~ std_normal()
      

