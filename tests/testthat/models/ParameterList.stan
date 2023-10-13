
data {
    real<lower=0> prior_alpha_inter;
    real<lower=0> prior_beta_inter;
    real prior_mu_myp;
    real<lower=0> prior_sigma_myp;
}

model {
    inter ~ gamma(prior_alpha_inter, prior_beta_inter);
    myp ~ normal(prior_mu_myp, prior_sigma_myp);
}



