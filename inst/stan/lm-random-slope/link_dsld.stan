
functions {
    //
    // Source - lm-random-slope/link_dsld.stan
    //

    // Derivative of SLD
    matrix link_dsld_contribution(
        matrix time,
        matrix link_function_inputs,
    ) {
        vector lm_rs_ind_rnd_slope = link_function_inputs[1];
        int nrows = rows(time);
        int ncols = cols(time);
        matrix[nrows, ncols] rnd_slope_mat = rep_matrix(lm_rs_ind_rnd_slope, ncols);
        matrix[nrows, ncols] result = rnd_slope_mat;
        return result;
    }
}


