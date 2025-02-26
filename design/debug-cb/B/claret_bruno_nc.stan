
functions {
    vector claret_bruno_mu(
        vector times,
        vector b,
        vector c,
        vector p,
        vector g
    ) {
        vector[rows(times)] values;
        values = exp(b) .* exp(
            (exp(g) .* times) -  (
                exp(p - c) .* (1 - exp(-exp(c) .* times))
            )
        );
        return values;
    }
}


data {
    int <lower=0> N_obs;
    int <lower=0> N_pt;
    vector[N_obs] values;
    vector[N_obs] times;
    array[N_obs] int<lower=1> pt_index;
}

parameters {

    real b_mu;
    real c_mu;
    real p_mu;
    real g_mu;

    real <lower=0> b_sigma;
    real <lower=0> c_sigma;
    real <lower=0> p_sigma;
    real <lower=0> g_sigma;

    real <lower=0> sigma;

    vector [N_pt] b_i;
    vector [N_pt] c_i;
    vector [N_pt] p_i;
    vector [N_pt] g_i;

}

transformed parameters {
    
    vector[N_obs] b_i_obs = b_i[pt_index];
    vector[N_obs] c_i_obs = c_i[pt_index];
    vector[N_obs] p_i_obs = p_i[pt_index];
    vector[N_obs] g_i_obs = g_i[pt_index];

    vector[N_obs] mu = claret_bruno_mu(times, b_i_obs, c_i_obs, p_i_obs, g_i_obs);
}

// pars <- list(
//     b = 60,
//     g = 0.5,
//     c = 0.4,
//     p = 0.7,
//     sigma = 0.004
// )

model {
    b_mu ~ normal(log(60), 0.5);
    c_mu ~ normal(log(0.5), 0.5);
    p_mu ~ normal(log(0.4), 0.5);
    g_mu ~ normal(log(0.7), 0.5);

    b_sigma ~ lognormal(log(0.1), 0.5);
    c_sigma ~ lognormal(log(0.1), 0.5);
    p_sigma ~ lognormal(log(0.1), 0.5);
    g_sigma ~ lognormal(log(0.1), 0.5);

    b_i ~ normal(b_mu, b_sigma);
    c_i ~ normal(c_mu, c_sigma);
    p_i ~ normal(p_mu, p_sigma);
    g_i ~ normal(g_mu, g_sigma);

    sigma ~ lognormal(log(0.02), 0.5);
    values ~ normal(mu, mu * sigma);
}




