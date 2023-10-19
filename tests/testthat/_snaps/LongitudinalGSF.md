# Print method for LongitudinalGSF works as expected

    Code
      x <- LongitudinalGSF()
      print(x)
    Output
      
      Generalized Stein-Fojo Longitudinal Model with parameters:
          lm_gsf_mu_bsld ~ lognormal(mu = 4.00733318523247, sigma = 5)
          lm_gsf_mu_ks ~ lognormal(mu = -2.30258509299405, sigma = 0.5)
          lm_gsf_mu_kg ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_mu_phi ~ beta(a = 2, b = 8)
          lm_gsf_omega_bsld ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_omega_ks ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_omega_kg ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_omega_phi ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_sigma ~ lognormal(mu = -2.30258509299405, sigma = 0.8)
          lm_gsf_eta_tilde_bsld ~ std_normal()
          lm_gsf_eta_tilde_ks ~ std_normal()
          lm_gsf_eta_tilde_kg ~ std_normal()
          lm_gsf_eta_tilde_phi ~ std_normal()
      

---

    Code
      x <- LongitudinalGSF(sigma = prior_normal(0, 1), mu_phi = prior_gamma(2, 1))
      print(x)
    Output
      
      Generalized Stein-Fojo Longitudinal Model with parameters:
          lm_gsf_mu_bsld ~ lognormal(mu = 4.00733318523247, sigma = 5)
          lm_gsf_mu_ks ~ lognormal(mu = -2.30258509299405, sigma = 0.5)
          lm_gsf_mu_kg ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_mu_phi ~ gamma(alpha = 2, beta = 1)
          lm_gsf_omega_bsld ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_omega_ks ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_omega_kg ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_omega_phi ~ lognormal(mu = -2.30258509299405, sigma = 1)
          lm_gsf_sigma ~ normal(mu = 0, sigma = 1)
          lm_gsf_eta_tilde_bsld ~ std_normal()
          lm_gsf_eta_tilde_ks ~ std_normal()
          lm_gsf_eta_tilde_kg ~ std_normal()
          lm_gsf_eta_tilde_phi ~ std_normal()
      

