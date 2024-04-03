


transformed parameters {
    //
    // Source - lm-stein-fojo/link.stan
    //
    matrix[n_subjects, 3] link_function_inputs;
    link_function_inputs[,1] = lm_sf_psi_bsld;
    link_function_inputs[,2] = lm_sf_psi_ks;
    link_function_inputs[,3] = lm_sf_psi_kg;
}



