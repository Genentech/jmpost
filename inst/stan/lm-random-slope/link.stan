
transformed parameters {
    //
    // Source - lm-random-slope/link.stan
    //
    matrix[Nind, 1] link_function_inputs = to_matrix(lm_rs_ind_rnd_slope);
}
