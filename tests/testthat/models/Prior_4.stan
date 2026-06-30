data {
    int<lower=1> prior_dim_mus_bob;
    int<lower=1> prior_dim_sigmas_bob;
    vector[prior_dim_mus_bob] prior_mus_bob;
    vector<lower=0>[prior_dim_sigmas_bob] prior_sigmas_bob;
}

model {
    bob ~ normal(prior_mus_bob, prior_sigmas_bob);
}
