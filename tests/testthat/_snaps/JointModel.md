# JointModel snapshots assembled parameter declarations

    Code
      snapshot_joint_model_parameter_decl(JointModel(longitudinal = LongitudinalRandomSlope(),
      survival = SurvivalWeibullPH(), link = linkDSLD()))
    Output
      parameters
      vector[n_studies] lm_rs_intercept;
      vector[n_arms] lm_rs_slope_mu;
      vector<lower=0>[n_arms] lm_rs_slope_sigma;
      real<lower=0> lm_rs_sigma;
      vector[n_subjects] lm_rs_ind_rnd_slope;
      real<lower=0> sm_weibull_ph_lambda;
      real<lower=0> sm_weibull_ph_gamma;
      vector[p_os_cov_design] beta_os_cov;
      real link_dsld;
      
      transformed parameter constants
      <none>

---

    Code
      snapshot_joint_model_parameter_decl(JointModel(longitudinal = LongitudinalGSF(
        centred = FALSE), survival = SurvivalWeibullPH(), link = Link(linkTTG(),
      linkDSLD(), linkGrowth())))
    Output
      parameters
      vector[n_studies] lm_gsf_mu_bsld;
      vector[n_arms] lm_gsf_mu_ks;
      vector[n_arms] lm_gsf_mu_kg;
      vector[n_arms] lm_gsf_mu_phi;
      vector<lower=0>[n_studies] lm_gsf_omega_bsld;
      vector<lower=0>[n_arms] lm_gsf_omega_ks;
      vector<lower=0>[n_arms] lm_gsf_omega_kg;
      vector<lower=0>[n_arms] lm_gsf_omega_phi;
      real<lower=0> lm_gsf_sigma;
      vector[n_subjects] lm_gsf_eta_tilde_bsld;
      vector[n_subjects] lm_gsf_eta_tilde_ks;
      vector[n_subjects] lm_gsf_eta_tilde_kg;
      vector[n_subjects] lm_gsf_eta_tilde_phi;
      real<lower=0> sm_weibull_ph_lambda;
      real<lower=0> sm_weibull_ph_gamma;
      vector[p_os_cov_design] beta_os_cov;
      real link_ttg;
      real link_dsld;
      real link_growth;
      
      transformed parameter constants
      <none>

---

    Code
      snapshot_joint_model_parameter_decl(JointModel(survival = SurvivalExponential(
        lambda = prior_const(0.5))))
    Output
      parameters
      vector[p_os_cov_design] beta_os_cov;
      
      transformed parameter constants
      real<lower=0, upper=8000> sm_exp_lambda = prior_const_sm_exp_lambda;

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
           Random Slope Longitudinal Model (additive error) with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 1, sigma = 3)
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
           Random Slope Longitudinal Model (additive error) with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 1, sigma = 3)
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
           Generalized Stein-Fojo Longitudinal Model (additive error) with parameters:
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
           Random Slope Longitudinal Model (additive error) with parameters:
               lm_rs_intercept ~ normal(mu = 30, sigma = 10)
               lm_rs_slope_mu ~ normal(mu = 1, sigma = 3)
               lm_rs_slope_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_sigma ~ lognormal(mu = 0, sigma = 1.5)
               lm_rs_ind_rnd_slope ~ <None>
           
        Link:     
           No Link

