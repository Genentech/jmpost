
transformed parameters {
    //
    // Source - lm-random-slope/link.stan
    //
    
    matrix[Nind, 2] link_function_inputs;
    link_function_inputs[, 1] = rep_vector(lm_rs_intercept, Nind);
    link_function_inputs[, 2] = lm_rs_ind_rnd_slope;
}
