transformed parameters {
    matrix[n_subjects, 2] link_function_inputs;
    link_function_inputs[, 1] = lm_rsc_ind_intercept;
    link_function_inputs[, 2] = lm_rsc_ind_rnd_slope;
}
