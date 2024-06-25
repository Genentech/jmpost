
transformed parameters {
    //
    // Source - lm-random-slope/link.stan
    //
    
    matrix[n_subjects, 2] link_function_inputs;
    link_function_inputs[, 1] = lm_rs_ind_intercept;
    link_function_inputs[, 2] = lm_rs_ind_rnd_slope;
}
