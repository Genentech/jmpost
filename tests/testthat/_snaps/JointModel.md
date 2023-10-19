# JointModel print method works as expected

    Code
      x <- JointModel(longitudinal = LongitudinalRandomSlope(), survival = SurvivalWeibullPH(),
      link = LinkRandomSlope())
      print(x)
    Output
      
      A Joint Model with:
      
        Survival:     
           Weibull-PH Survival Model with parameters:
               sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
               sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
               beta_os_cov ~ normal(mu = 0, sigma = 5)
           
        Longitudinal:     
           Random Slope Longitudinal Model with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 0, sigma = 15)
               lm_rs_slope_sigma ~ lognormal(mu = 1, sigma = 5)
               lm_rs_sigma ~ lognormal(mu = 1, sigma = 5)
               lm_rs_ind_rnd_slope ~ <None>
           
        Link:     
           Random Slope Link with parameters:
               link_lm_phi ~ normal(mu = 0.2, sigma = 0.5)
           

---

    Code
      x <- JointModel(survival = SurvivalWeibullPH())
      print(x)
    Output
      
      A Joint Model with:
      
        Survival:     
           Weibull-PH Survival Model with parameters:
               sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
               sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
               beta_os_cov ~ normal(mu = 0, sigma = 5)
           
        Longitudinal:
           Not Specified
      
        Link:
           No Link
      

---

    Code
      x <- JointModel(longitudinal = LongitudinalGSF())
      print(x)
    Output
      
      A Joint Model with:
      
        Survival:
           Not Specified
      
        Longitudinal:     
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
           
        Link:
           No Link
      

---

    Code
      x <- JointModel(longitudinal = LongitudinalRandomSlope(), survival = SurvivalWeibullPH(),
      link = LinkNone())
      print(x)
    Output
      
      A Joint Model with:
      
        Survival:     
           Weibull-PH Survival Model with parameters:
               sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
               sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
               beta_os_cov ~ normal(mu = 0, sigma = 5)
           
        Longitudinal:     
           Random Slope Longitudinal Model with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 0, sigma = 15)
               lm_rs_slope_sigma ~ lognormal(mu = 1, sigma = 5)
               lm_rs_sigma ~ lognormal(mu = 1, sigma = 5)
               lm_rs_ind_rnd_slope ~ <None>
           
        Link:
           No Link
      

