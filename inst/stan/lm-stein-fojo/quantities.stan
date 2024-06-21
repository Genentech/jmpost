
functions {
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return sld(
            time,
            long_gq_parameters[,1],
            long_gq_parameters[,2],
            long_gq_parameters[,3]
        );
    }
}

generated quantities {
    //
    // Source - lm-stein-fojo/quantities.stan
    //
    matrix[n_subjects, 3] long_gq_parameters;
    long_gq_parameters[, 1] = lm_gsf_psi_bsld;
    long_gq_parameters[, 2] = lm_gsf_psi_ks;
    long_gq_parameters[, 3] = lm_gsf_psi_kg;

    matrix[gq_n_quant, 3] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = exp(lm_sf_mu_bsld[gq_long_pop_study_index]);
    long_gq_pop_parameters[, 2] = exp(lm_sf_mu_ks[gq_long_pop_arm_index]);
    long_gq_pop_parameters[, 3] = exp(lm_sf_mu_kg[gq_long_pop_arm_index]);
}
