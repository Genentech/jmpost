


transformed parameters {
    //
    // Source - lm-claret-bruno/link.stan
    //
    matrix[n_subjects, 4] link_function_inputs;
    link_function_inputs[,1] = lm_clbr_ind_b;
    link_function_inputs[,2] = lm_clbr_ind_g;
    link_function_inputs[,3] = lm_clbr_ind_c;
    link_function_inputs[,4] = lm_clbr_ind_p;
}

