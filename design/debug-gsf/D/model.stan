
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
    vector<lower=0.00000000001>[Nind] ks;
    vector<lower=0.00000000001>[Nind] kg;
    vector<lower=0.00000000001>[Nind] kb;
    vector<lower=0.00000000001, upper=0.999999999>[Nind] phi;
    real<lower=0.00000000001> sigma;
}


model {
    vector[N] Ypred;
    
    ks ~ lognormal(log(0.6), 0.3);
    kg ~ lognormal(log(0.2), 0.3);
    kb ~ lognormal(log(60), 1);
    phi ~ beta(2, 2);
    
    Ypred = sld(
        Tobs,
        kb[ind_index],
        ks[ind_index],
        kg[ind_index],
        phi[ind_index]
    );
    
    Yobs ~ normal(Ypred, Ypred .* sigma);
}

generated quantities {
   real ks_mu = mean(ks);
   real kg_mu = mean(kg);
   real phi_mu = mean(phi);
   real b_mu = mean(kb);
}
