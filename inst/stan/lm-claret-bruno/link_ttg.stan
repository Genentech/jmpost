

functions {
    //
    // Source - lm-claret-bruno/link_ttg.stan
    //
    matrix link_ttg_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int nrows = rows(link_function_inputs);
        int ncols = cols(time);
        vector[nrows] ind_g = link_function_inputs[,2];
        vector[nrows] ind_c = link_function_inputs[,3];
        vector[nrows] ind_p = link_function_inputs[,4];
        return rep_matrix(
            log(ind_p ./ ind_p) ./ ind_c,
            ncols
        );
    }
}


