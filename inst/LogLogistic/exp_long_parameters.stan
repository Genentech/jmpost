real<lower=0, upper=100> mean_mu_ks;
  real<lower=0, upper=100> mean_mu_kg;
  real<lower=0.001, upper=0.999> mean_mu_phi;
  real<lower=0, upper=10> sd_mu_ks;
  real<lower=0, upper=10> sd_mu_kg;
  real<lower=0, upper=10> sd_mu_phi;

  // Population parameters.
  row_vector<lower=0>[n_studies] mu_bsld;
  row_vector<lower=0>[n_arms] mu_ks;
  row_vector<lower=0>[n_arms] mu_kg;
  row_vector<lower=0, upper=1>[n_arms] mu_phi;

  real<lower=0> omega_bsld;
  real<lower=0> omega_ks;
  real<lower=0> omega_kg;
  real<lower=0> omega_phi;

  // Standard deviation for RUV.
  real<lower=0.00001, upper=100> sigma;

  // Random effects.
  row_vector[Nind] eta_tilde_bsld;
  row_vector[Nind] eta_tilde_ks;
  row_vector[Nind] eta_tilde_kg;
  row_vector[Nind] eta_tilde_phi;
