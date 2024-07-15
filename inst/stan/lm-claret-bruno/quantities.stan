
functions {
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return sld(
            time,
            long_gq_parameters[,1],
            long_gq_parameters[,2],
            long_gq_parameters[,3],
            long_gq_parameters[,4]
        );
    }
}

generated quantities {
    //
    // Source - lm-claret-bruno/quantities.stan
    //
    matrix[n_subjects, 4] long_gq_parameters;
    long_gq_parameters[, 1] = lm_clbr_ind_b;
    long_gq_parameters[, 2] = lm_clbr_ind_g;
    long_gq_parameters[, 3] = lm_clbr_ind_c;
    long_gq_parameters[, 4] = lm_clbr_ind_p;
    
    
    matrix[gq_n_quant, 4] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = exp(lm_clbr_mu_b[gq_long_pop_study_index]);
    long_gq_pop_parameters[, 2] = exp(lm_clbr_mu_g[gq_long_pop_arm_index]);
    long_gq_pop_parameters[, 3] = exp(lm_clbr_mu_c[gq_long_pop_arm_index]);
    long_gq_pop_parameters[, 3] = exp(lm_clbr_mu_p[gq_long_pop_arm_index]);
}
