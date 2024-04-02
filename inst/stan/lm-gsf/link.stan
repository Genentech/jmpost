


transformed parameters {
    //
    // Source - lm-gsf/link.stan
    //
    matrix[n_subjects, 4] link_function_inputs;
    link_function_inputs[,1] = lm_gsf_psi_bsld;
    link_function_inputs[,2] = lm_gsf_psi_ks;
    link_function_inputs[,3] = lm_gsf_psi_kg;
    link_function_inputs[,4] = lm_gsf_psi_phi;
}



