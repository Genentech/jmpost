
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
    real<lower=0.00000000001> mu_b;
    real<lower=0.00000000001> mu_s;
    real<lower=0.00000000001> mu_g;

    real<lower=0.00000000001> sigma_b;
    real<lower=0.00000000001> sigma_s;
    real<lower=0.00000000001> sigma_g;

    real<lower=0.00000000001> a_phi;
    real<lower=0.00000000001> b_phi;

    real<lower=0.00000000001> sigma;

    vector<lower=0.00000000001>[Nind] ind_b;
    vector<lower=0.00000000001>[Nind] ind_s;
    vector<lower=0.00000000001>[Nind] ind_g;
    vector<lower=0.00000000001, upper=0.999999999>[Nind] ind_phi;

}

model {
    vector[N] Ypred;
    
    
    mu_b ~ lognormal(log(60), 0.4);
    mu_s ~ lognormal(log(0.6), 0.4);
    mu_g ~ lognormal(log(0.25), 0.4);
    
    sigma_b ~ lognormal(log(0.4), 0.3);
    sigma_s ~ lognormal(log(0.2), 0.3);
    sigma_g ~ lognormal(log(0.2), 0.3);
    
    a_phi ~ lognormal(log(2), 0.3);
    b_phi ~ lognormal(log(2), 0.3);
    
    sigma ~ lognormal(log(0.05), 0.3);

    ind_b ~ lognormal(log(mu_b), sigma_b);
    ind_s ~ lognormal(log(mu_s), sigma_s);
    ind_g ~ lognormal(log(mu_g), sigma_g);
    ind_phi ~ beta(a_phi, b_phi);

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
   real b_mean = mu_b * exp((sigma_b^2)/2);
   real s_mean = mu_s * exp((sigma_s^2)/2);
   real g_mean = mu_g * exp((sigma_g^2)/2);
   real phi_mean = a_phi / (a_phi + b_phi);
}
