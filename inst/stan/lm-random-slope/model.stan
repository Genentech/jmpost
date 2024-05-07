
functions {
    //
    // Source - lm-random-slope/model.stan
    //
    vector lm_predict_individual_patient(vector time, matrix long_gq_parameters) {
        int nrow = rows(time);
        return (
            long_gq_parameters[, 1] + long_gq_parameters[, 2] .* time
        );
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
    vector[n_subjects] lm_rs_ind_intercept = to_vector(lm_rs_intercept[subject_study_index]);
    vector[n_tumour_all] lm_rs_rslope_ind = to_vector(lm_rs_ind_rnd_slope[subject_tumour_index]);

    vector[n_tumour_all] Ypred = lm_rs_ind_intercept[subject_tumour_index] + lm_rs_rslope_ind .* tumour_time;

    Ypred_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        rep_vector(lm_rs_sigma, n_tumour_obs)
    );
    if (n_tumour_cens > 0 ) {
        Ypred_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
            tumour_value_lloq,
            Ypred[subject_tumour_index_cens],
            rep_vector(lm_rs_sigma, n_tumour_cens)
        );
    }
}


model {
    //
    // Source - lm-random-slope/model.stan
    //

    lm_rs_ind_rnd_slope ~ normal(
        lm_rs_slope_mu[subject_arm_index],
        lm_rs_slope_sigma
    );
}


generated quantities {
    //
    // Source - lm-random-slope/model.stan
    //
    matrix[n_subjects, 2] long_gq_parameters;
    long_gq_parameters[, 1] = lm_rs_ind_intercept;
    long_gq_parameters[, 2] = lm_rs_ind_rnd_slope;


    matrix[gq_n_quant, 2] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = to_vector(lm_rs_intercept[gq_long_pop_study_index]);
    long_gq_pop_parameters[, 2] = to_vector(lm_rs_slope_mu[gq_long_pop_arm_index]);
}
