
functions {
    //
    // Source - lm-claret-bruno/link_identity.stan
    //
    matrix link_identity_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int nrows = rows(link_function_inputs);
        int ncols = cols(time);
        vector[nrows] ind_b = link_function_inputs[,1];
        vector[nrows] ind_g = link_function_inputs[,2];
        vector[nrows] ind_c = link_function_inputs[,3];
        vector[nrows] ind_p = link_function_inputs[,4];
        matrix[nrows, ncols] result;
        for (i in 1:ncols) {
            result[,i] = fmin(
                sld(time[,i], ind_b, ind_g, ind_c, ind_p),
                8000.0
            );
        }
        return result;
    }
}
