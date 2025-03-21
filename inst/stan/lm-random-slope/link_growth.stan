
functions {
    //
    // Source - lm-random-slope/link_dsld.stan
    //
    matrix link_growth_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int nrows = rows(time);
        int ncols = cols(time);
        vector[nrows] lm_rs_ind_rnd_slope = link_function_inputs[,2];
        matrix[nrows, ncols] rnd_slope_mat = rep_matrix(lm_rs_ind_rnd_slope, ncols);
        return rnd_slope_mat;
    }
}


