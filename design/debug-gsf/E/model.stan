
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
    real<lower=0.00000000001, upper=0.999999999> mu_phi;

    vector[Nind] eta_b;
    vector[Nind] eta_s;
    vector[Nind] eta_g;
    vector[Nind] eta_phi;
    
    real<lower=0.00000000001> sigma;
}


transformed parameters {
    vector<lower=0.00000000001>[Nind] ind_b;
    vector<lower=0.00000000001>[Nind] ind_s;
    vector<lower=0.00000000001>[Nind] ind_g;
    vector<lower=0.00000000001, upper=0.999999999>[Nind] ind_phi;

    ind_b = exp(log(mu_b) + eta_b);
    ind_s = exp(log(mu_s) + eta_s);
    ind_g = exp(log(mu_g) + eta_g);
    ind_phi = inv_logit(logit(mu_phi) + eta_phi);
}


model {
    vector[N] Ypred;
    
    
    mu_b ~ lognormal(log(60), 1);
    //mu_b ~ normal(60, 0.0001);
    mu_s ~ lognormal(log(0.5), 0.3);
    mu_g ~ lognormal(log(0.2), 0.3);
    mu_phi ~ beta(3, 3);
    
    eta_b ~ normal(0, 1);
    eta_s ~ normal(0, 0.3);
    eta_g ~ normal(0, 0.3);
    eta_phi ~ normal(0, 2);
    
    sigma ~ lognormal(log(0.3), 0.2);

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
   real eta_b_mean = mean(eta_b);
   real eta_s_mean = mean(eta_s);
   real eta_g_mean = mean(eta_g);
   real eta_phi_mean = mean(eta_phi);
}
