data {
    real prior_mu_tim;
    real<lower=0> prior_sigma_tim;
}
model {
    tim ~ lognormal(prior_mu_tim, prior_sigma_tim);
}
