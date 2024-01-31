data {
    real prior_mu_tom;
    real<lower=0> prior_sigma_tom;
    real<lower=0> prior_alpha_dave;
    real<lower=0> prior_beta_dave;
    real<lower=0> prior_alpha_jim;
    real<lower=0> prior_beta_jim;
    real<lower=0> prior_nu_ben;
    real prior_mu_ben;
    real<lower=0> prior_sigma_ben;
    real prior_alpha_kim;
    real prior_beta_kim;
}

parameters {
    real tom;
    real dave;
    real jim;
    real ben;
    real kim;
}

model {
    tom ~ logistic(prior_mu_tom, prior_sigma_tom);
    dave ~ loglogistic(prior_alpha_dave, prior_beta_dave);
    jim ~ inv_gamma(prior_alpha_jim, prior_beta_jim);
    ben ~ student_t(prior_nu_ben, prior_mu_ben, prior_sigma_ben);
    kim ~ uniform(prior_alpha_kim, prior_beta_kim);
}
