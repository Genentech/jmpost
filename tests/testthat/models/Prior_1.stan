data {
    real prior_mu_bob;
    real<lower=0> prior_sigma_bob;
}
model {
    bob ~ normal(prior_mu_bob, prior_sigma_bob);
}
