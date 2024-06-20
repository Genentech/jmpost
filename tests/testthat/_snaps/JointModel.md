# JointModel print method works as expected

    Code
      x <- JointModel(longitudinal = LongitudinalRandomSlope(), survival = SurvivalWeibullPH(),
      link = linkDSLD())
      print(x)
    Output
      
      A Joint Model with:
      
        Survival:     
           Weibull-PH Survival Model with parameters:
               sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
               sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
               beta_os_cov ~ normal(mu = 0, sigma = 2)
           
        Longitudinal:     
           Random Slope Longitudinal Model with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 0, sigma = 15)
               lm_rs_slope_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_ind_rnd_slope ~ <None>
           
        Link:     
           Link with the following components/parameters:
               link_dsld ~ normal(mu = 0, sigma = 2)

---

    Code
      x <- JointModel(longitudinal = LongitudinalRandomSlope(), survival = SurvivalWeibullPH(),
      link = Link(linkDSLD(), linkIdentity()))
      print(x)
    Output
      
      A Joint Model with:
      
        Survival:     
           Weibull-PH Survival Model with parameters:
               sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
               sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
               beta_os_cov ~ normal(mu = 0, sigma = 2)
           
        Longitudinal:     
           Random Slope Longitudinal Model with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 0, sigma = 15)
               lm_rs_slope_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_ind_rnd_slope ~ <None>
           
        Link:     
           Link with the following components/parameters:
               link_dsld ~ normal(mu = 0, sigma = 2)
               link_identity ~ normal(mu = 0, sigma = 2)

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
               beta_os_cov ~ normal(mu = 0, sigma = 2)
           
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
               lm_gsf_mu_bsld ~ normal(mu = 4.09434, sigma = 1)
               lm_gsf_mu_ks ~ normal(mu = -0.69315, sigma = 1)
               lm_gsf_mu_kg ~ normal(mu = -1.20397, sigma = 1)
               lm_gsf_omega_bsld ~ lognormal(mu = -1.60944, sigma = 1)
               lm_gsf_omega_ks ~ lognormal(mu = -1.60944, sigma = 1)
               lm_gsf_omega_kg ~ lognormal(mu = -1.60944, sigma = 1)
               lm_gsf_a_phi ~ lognormal(mu = 1.60944, sigma = 1)
               lm_gsf_b_phi ~ lognormal(mu = 1.60944, sigma = 1)
               lm_gsf_psi_phi ~ <None>
               lm_gsf_sigma ~ lognormal(mu = -2.30259, sigma = 1)
               lm_gsf_eta_tilde_bsld ~ std_normal()
               lm_gsf_eta_tilde_ks ~ std_normal()
               lm_gsf_eta_tilde_kg ~ std_normal()
           
        Link:     
           No Link

---

    Code
      x <- JointModel(longitudinal = LongitudinalRandomSlope(), survival = SurvivalWeibullPH(),
      link = Link())
      print(x)
    Output
      
      A Joint Model with:
      
        Survival:     
           Weibull-PH Survival Model with parameters:
               sm_weibull_ph_lambda ~ gamma(alpha = 2, beta = 0.5)
               sm_weibull_ph_gamma ~ gamma(alpha = 2, beta = 0.5)
               beta_os_cov ~ normal(mu = 0, sigma = 2)
           
        Longitudinal:     
           Random Slope Longitudinal Model with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 0, sigma = 15)
               lm_rs_slope_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_ind_rnd_slope ~ <None>
           
        Link:     
           No Link

