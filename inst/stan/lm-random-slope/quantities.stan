
functions {
    //
    // Source - lm-random-slope/quantities.stan
    //
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        int nrow = rows(time);
        return (
            long_gq_parameters[, 1] + long_gq_parameters[, 2] .* time
        );
    }
}

generated quantities {
    //
    // Source - lm-random-slope/quantities.stan
    //
    matrix[n_subjects, 2] long_gq_parameters;
    long_gq_parameters[, 1] = lm_rs_ind_intercept;
    long_gq_parameters[, 2] = lm_rs_ind_rnd_slope;


    matrix[gq_n_quant, 2] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = to_vector(lm_rs_intercept[gq_long_pop_study_index]);
    long_gq_pop_parameters[, 2] = to_vector(lm_rs_slope_mu[gq_long_pop_arm_index]);
}
