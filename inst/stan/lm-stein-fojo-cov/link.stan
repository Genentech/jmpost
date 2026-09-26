transformed parameters {
    matrix[n_subjects, 3] link_function_inputs;
    link_function_inputs[, 1] = lm_sfc_psi_b;
    link_function_inputs[, 2] = lm_sfc_psi_s;
    link_function_inputs[, 3] = lm_sfc_psi_g;
}
