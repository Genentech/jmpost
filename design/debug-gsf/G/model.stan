
functions {
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg
    ) {
        vector[num_elements(time)] result = fmin(
            8000.0,
            psi_bsld .* ( exp(- psi_ks .* time) +  exp(psi_kg .* time) - 1)
        );
        return result;
    }
}

data {
    int N;
    int Nind;
    vector[N] Yobs;
    vector[N] Tobs;
    array[N] int ind_index;
}

parameters {
    real<lower=0.00000000001> mu_b;
    real<lower=0.00000000001> mu_s;
    real<lower=0.00000000001> mu_g;

    real<lower=0.00000000001> sigma_b;
    real<lower=0.00000000001> sigma_s;
    real<lower=0.00000000001> sigma_g;
    
    vector[Nind] LB;
    vector[Nind] LS;
    vector[Nind] LG;
    
    real<lower=0.00000000001> sigma;
}


transformed parameters {
    vector<lower=0.00000000001>[Nind] ind_b;
    vector<lower=0.00000000001>[Nind] ind_s;
    vector<lower=0.00000000001>[Nind] ind_g;

    ind_b = exp(LB);
    ind_s = exp(LS);
    ind_g = exp(LG);
}


model {
    vector[N] Ypred;

    mu_b ~ lognormal(log(60), 0.5);
    mu_s ~ lognormal(log(0.6), 0.3);
    mu_g ~ lognormal(log(0.2), 0.3);

    sigma_b ~ lognormal(log(0.3), 0.2);
    sigma_s ~ lognormal(log(0.3), 0.2);
    sigma_g ~ lognormal(log(0.3), 0.2);
    
    sigma ~ lognormal(log(0.05), 0.05);
    
    LB ~ normal(mu_b, sigma_b);
    LS ~ normal(mu_s, sigma_s);
    LG ~ normal(mu_g, sigma_g);

    Ypred = sld(
        Tobs,
        ind_b[ind_index],
        ind_s[ind_index],
        ind_g[ind_index]
    );
    
    Yobs ~ normal(Ypred, sigma);
}

generated quantities {
   real b_mean = exp(mu_b + (sigma_b^2)/2);
   real s_mean = exp(mu_s + (sigma_s^2)/2);
   real g_mean = exp(mu_g + (sigma_g^2)/2);
}
