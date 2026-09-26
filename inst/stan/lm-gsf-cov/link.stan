transformed parameters {
    matrix[n_subjects, 4] link_function_inputs;
    link_function_inputs[, 1] = lm_gsfc_psi_b;
    link_function_inputs[, 2] = lm_gsfc_psi_s;
    link_function_inputs[, 3] = lm_gsfc_psi_g;
    link_function_inputs[, 4] = lm_gsfc_psi_phi;
}
