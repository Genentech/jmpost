
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
    real mu_b;
    real mu_s;
    real mu_g;

    real<lower=0.0000000000001> sigma_b;
    real<lower=0.0000000000001> sigma_s;
    real<lower=0.0000000000001> sigma_g;

    vector[Nind] raw_b;
    vector[Nind] raw_s;
    vector[Nind] raw_g;

    real<lower=0.00000000001> sigma;
    
    vector<lower=0.0000000000001, upper=0.999999999999>[Nind] ind_phi;
    real<lower=0.0000000000001> a_phi;
    real<lower=0.0000000000001> b_phi;
    
}


transformed parameters {
    vector<lower=0.00000000001>[Nind] ind_b;
    vector<lower=0.00000000001>[Nind] ind_s;
    vector<lower=0.00000000001>[Nind] ind_g;

    ind_b = exp(mu_b + raw_b * sigma_b);
    ind_s = exp(mu_s + raw_s * sigma_s);
    ind_g = exp(mu_g + raw_g * sigma_g);
}


model {
    vector[N] Ypred;

    mu_b ~ normal(log(60), 1);
    mu_s ~ normal(log(0.7), 1);
    mu_g ~ normal(log(0.3), 1);

    
    raw_b ~ std_normal();
    raw_s ~ std_normal();
    raw_g ~ std_normal();


    sigma_b ~ lognormal(log(0.3), 1);
    sigma_s ~ lognormal(log(0.3), 1);
    sigma_g ~ lognormal(log(0.3), 1);

    ind_phi ~ beta(a_phi, b_phi);
    a_phi ~ lognormal(log(2), 1);
    b_phi ~ lognormal(log(2), 1);


    sigma ~ lognormal(log(0.05), 1);

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

