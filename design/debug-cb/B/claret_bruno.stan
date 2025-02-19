
functions {
    vector claret_bruno_mu(
        vector times,
        vector b,
        vector c,
        vector p,
        vector g
    ) {
        vector[rows(times)] values;
        values = b .* exp(
            (g .* times) -  (
                (p./c) .* (1 - exp(-c .* times))
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

    real <lower=0> b_mu;
    real <lower=0> c_mu;
    real <lower=0> p_mu;
    real <lower=0> g_mu;

    real <lower=0> b_sigma;
    real <lower=0> c_sigma;
    real <lower=0> p_sigma;
    real <lower=0> g_sigma;

    real <lower=0> sigma;

    vector <lower=0>[N_pt] b_i;
    vector <lower=0>[N_pt] c_i;
    vector <lower=0>[N_pt] p_i;
    vector <lower=0>[N_pt] g_i;

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
    b_mu ~ lognormal(log(60), 0.5);
    c_mu ~ lognormal(log(0.5), 0.5);
    p_mu ~ lognormal(log(0.4), 0.5);
    g_mu ~ lognormal(log(0.7), 0.5);

    b_sigma ~ lognormal(log(0.1), 0.5);
    c_sigma ~ lognormal(log(0.1), 0.5);
    p_sigma ~ lognormal(log(0.1), 0.5);
    g_sigma ~ lognormal(log(0.1), 0.5);

    b_i ~ lognormal(log(b_mu), b_sigma);
    c_i ~ lognormal(log(c_mu), c_sigma);
    p_i ~ lognormal(log(p_mu), p_sigma);
    g_i ~ lognormal(log(g_mu), g_sigma);

    sigma ~ lognormal(log(0.02), 0.5);
    values ~ normal(mu, mu * sigma);
}




