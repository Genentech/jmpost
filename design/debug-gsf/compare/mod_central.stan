
functions {
    // Vectorized ifelse() similar as in R.
    vector ifelse(array[] int condition, vector yes, vector no) {
        vector[num_elements(yes)] result;
        for (i in 1:num_elements(yes)) {
            result[i] = condition[i] ? yes[i] : no[i];
        }
        return result;
    }

    // Vectorized negative predicate. Basically returns ifelse(x < 0, 1, 0).
    array[] int is_negative(vector x) {
        array[num_elements(x)] int result;
        for (i in 1:num_elements(x)) {
            result[i] = x[i] < 0.0;
        }
        return result;
    }
    
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg,
        vector psi_phi
    ) {
        vector[num_elements(time)] psi_phi_mod = ifelse(
            is_negative(time),
            zeros_vector(num_elements(time)),
            psi_phi
        );
        vector[num_elements(time)] result = fmin(
            8000.0,
            psi_bsld .* (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
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
    
    vector<lower=0.0000000000001>[Nind] ind_b;
    vector<lower=0.0000000000001>[Nind] ind_s;
    vector<lower=0.0000000000001>[Nind] ind_g;
    vector<lower=0.0000000000001, upper=0.999999999999>[Nind] ind_phi;
    
    real mu_b;
    real mu_s;
    real mu_g;
    
    real<lower=0.0000000000001> sigma_b;
    real<lower=0.0000000000001> sigma_s;
    real<lower=0.0000000000001> sigma_g;

    real<lower=0.0000000000001> a_phi;
    real<lower=0.0000000000001> b_phi;

    real<lower=0.00000000001> sigma;
}


model {
    vector[N] Ypred;

    ind_b ~ lognormal(mu_b, sigma_b);
    ind_s ~ lognormal(mu_s, sigma_s);
    ind_g ~ lognormal(mu_g, sigma_g);
    ind_phi ~ beta(a_phi, b_phi);
    
    mu_b ~ normal(log(60), 1);
    mu_s ~ normal(log(0.6), 1);
    mu_g ~ normal(log(0.2), 1);
    
    sigma_b ~ normal(log(0.3), 1);
    sigma_s ~ normal(log(0.3), 1);
    sigma_g ~ normal(log(0.3), 1);
    
    a_phi ~ lognormal(log(2), 1);
    b_phi ~ lognormal(log(2), 1);
    
    sigma ~ lognormal(log(0.05), 0.2);

    Ypred = sld(
        Tobs,
        ind_b[ind_index],
        ind_s[ind_index],
        ind_g[ind_index],
        ind_phi[ind_index]
    );
    
    Yobs ~ normal(Ypred, Ypred .* sigma);
}


generated quantities {
   real<lower = 0.000000000001> pop_mean_b = exp(mu_b + sigma_b^2/2);
   real<lower = 0.000000000001> pop_mean_s = exp(mu_s + sigma_s^2/2);
   real<lower = 0.000000000001> pop_mean_g = exp(mu_g + sigma_g^2/2);
}
