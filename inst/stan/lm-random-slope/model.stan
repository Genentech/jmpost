


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

    long_obvs_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        rep_vector(lm_rs_sigma, n_tumour_obs)
    );
    if (n_tumour_cens > 0 ) {
        long_obvs_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
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


