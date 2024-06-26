# Print method for LongitudinalClaretBruno works as expected

    Code
      x <- LongitudinalClaretBruno()
      print(x)
    Output
      
      Claret-Bruno Longitudinal Model with parameters:
          lm_clbr_mu_b ~ normal(mu = 4.09434, sigma = 0.5)
          lm_clbr_mu_g ~ normal(mu = 0, sigma = 0.5)
          lm_clbr_mu_c ~ normal(mu = -0.91629, sigma = 0.5)
          lm_clbr_mu_p ~ normal(mu = 0.69315, sigma = 0.5)
          lm_clbr_omega_b ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_omega_g ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_omega_c ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_omega_p ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_sigma ~ lognormal(mu = -2.30259, sigma = 0.5)
          lm_clbr_eta_b ~ std_normal()
          lm_clbr_eta_g ~ std_normal()
          lm_clbr_eta_c ~ std_normal()
          lm_clbr_eta_p ~ std_normal()
      

---

    Code
      x <- LongitudinalClaretBruno(sigma = prior_normal(0, 1), mu_g = prior_gamma(2,
        1))
      print(x)
    Output
      
      Claret-Bruno Longitudinal Model with parameters:
          lm_clbr_mu_b ~ normal(mu = 4.09434, sigma = 0.5)
          lm_clbr_mu_g ~ gamma(alpha = 2, beta = 1)
          lm_clbr_mu_c ~ normal(mu = -0.91629, sigma = 0.5)
          lm_clbr_mu_p ~ normal(mu = 0.69315, sigma = 0.5)
          lm_clbr_omega_b ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_omega_g ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_omega_c ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_omega_p ~ lognormal(mu = -1.60944, sigma = 0.5)
          lm_clbr_sigma ~ normal(mu = 0, sigma = 1)
          lm_clbr_eta_b ~ std_normal()
          lm_clbr_eta_g ~ std_normal()
          lm_clbr_eta_c ~ std_normal()
          lm_clbr_eta_p ~ std_normal()
      

