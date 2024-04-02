
functions {
    //
    // Source - lm-random-slope/model.stan
    //
    row_vector lm_predict_individual_patient(vector time, row_vector long_gq_parameters) {
        int nrow = rows(time);
        return (
            rep_vector(long_gq_parameters[1], nrow) + 
            rep_vector(long_gq_parameters[2], nrow) .* time
        )';
    }
}


parameters {
    //
    // Source - lm-random-slope/model.stan
    //

    array [n_studies] real lm_rs_intercept;
    array [n_arms] real lm_rs_slope_mu;
    real<lower={{ machine_double_eps }}> lm_rs_slope_sigma;
    real<lower={{ machine_double_eps }}> lm_rs_sigma;
    vector[n_subjects] lm_rs_ind_rnd_slope;
}


transformed parameters {
    //
    // Source - lm-random-slope/model.stan
    //
    vector[n_subjects] lm_rs_ind_intercept = to_vector(lm_rs_intercept[pt_study_index]);
    vector[Nta_total] lm_rs_rslope_ind = to_vector(lm_rs_ind_rnd_slope[ind_index]);

    vector[Nta_total] Ypred = lm_rs_ind_intercept[ind_index] + lm_rs_rslope_ind .* Tobs;

    Ypred_log_lik[obs_y_index] = vect_normal_log_dens(
        Yobs[obs_y_index],
        Ypred[obs_y_index],
        rep_vector(lm_rs_sigma, Nta_obs_y)
    );
    if (Nta_cens_y > 0 ) {
        Ypred_log_lik[cens_y_index] = vect_normal_log_cum(
            Ythreshold,
            Ypred[cens_y_index],
            rep_vector(lm_rs_sigma, Nta_cens_y)
        );
    }
}


model {
    //
    // Source - lm-random-slope/model.stan
    //

    lm_rs_ind_rnd_slope ~ normal(
        lm_rs_slope_mu[pt_arm_index],
        lm_rs_slope_sigma
    );
}


generated quantities {
    //
    // Source - lm-random-slope/model.stan
    //
    matrix[Nind, 2] long_gq_parameters;
    long_gq_parameters[, 1] = lm_rs_ind_intercept;
    long_gq_parameters[, 2] = lm_rs_ind_rnd_slope;
}
