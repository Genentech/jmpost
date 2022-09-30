  real ypred_ij;
  real yobs_ij;

  // Hyper priors definition.
  mean_mu_ks ~ lognormal(1,0.5); // log(3)
  mean_mu_kg ~ lognormal(-0.36,1); // log(0.7)
  mean_mu_phi ~ beta(5,5);
  sd_mu_ks ~ lognormal(0,0.5);
  sd_mu_kg ~ lognormal(0,0.5);
  sd_mu_phi ~ lognormal(0,0.5);

  // Priors definition.
  mu_bsld ~ lognormal(55,5);

  mu_ks[sld_par_shared] ~ lognormal(mean_mu_ks, sd_mu_ks);
  mu_kg[sld_par_shared] ~ lognormal(mean_mu_kg, sd_mu_kg);
  logit(mu_phi[sld_par_shared]) ~ normal(logit(mean_mu_phi), sd_mu_phi);

  mu_ks[sld_par_separate] ~ lognormal(1,0.5);
  mu_kg[sld_par_separate] ~ lognormal(-0.36,1);
  mu_phi[sld_par_separate] ~ beta(5,5);

  omega_bsld ~ lognormal(0,1);
  omega_ks ~ lognormal(0,1);
  omega_kg ~ lognormal(0,1);
  omega_phi ~ lognormal(0,1);

  sigma ~ lognormal(-1.6,0.8);

  eta_tilde_bsld ~ normal(0,5);
  eta_tilde_ks ~ normal(0,5);
  eta_tilde_kg ~ normal(0,5);
  eta_tilde_phi ~ normal(0,5);
