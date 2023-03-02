functions {
    vector ifelse(array[] int condition, vector yes, vector no) {
        vector[num_elements(yes)] result;
        for (i in 1:num_elements(yes)) {
            result[i] = condition[i] ? yes[i] : no[i];
        }
        return result;
    }

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
    real y_mean;
    int sld_n;
    vector[sld_n] sld_time;
    vector[sld_n] sld_bsld;
    vector[sld_n] sld_s;
    vector[sld_n] sld_g;
    vector[sld_n] sld_phi;
    
    int ife_n;
    array[ife_n] int ife_cond;
    vector[ife_n] yes;
    vector[ife_n] no;
    
    int is_neg_n;
    vector[is_neg_n] is_neg_x;
}

parameters {
    real y;
}

model {
    y ~normal(y_mean, 1);
}



generated quantities {
    vector[sld_n] sld_results = sld(sld_time, sld_bsld, sld_s, sld_g, sld_phi);

    vector[ife_n] ife_results = ifelse(ife_cond, ife_yes, ife_no);
    
    vector[is_neg_n] is_neg_results = is_negative(is_neg_x);

}




