
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
        real psi_ks,
        real psi_kg,
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
    vector[Nind] bsld;
    vector[N] Yobs;
    vector[N] Tobs;
    array[N] int ind_index;
}

parameters {
    real<lower=0.00000000001> ks;
    real<lower=0.00000000001> kg;
    real<lower=0.00000000001> sigma;
}


model {
    vector[N] Ypred;
    
    ks ~ lognormal(log(0.4), 0.5);
    kg ~ lognormal(log(0.4), 0.5);
    
    Ypred = sld(
        Tobs,
        bsld[ind_index],
        ks,
        kg,
        rep_vector(0.5, N)
    );
    
    Yobs ~ normal(Ypred, Ypred .* sigma);
}
