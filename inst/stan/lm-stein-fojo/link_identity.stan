
functions {
    //
    // Source - lm-stein-fojo/link_identity.stan
    //
    matrix link_identity_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int nrows = rows(link_function_inputs);
        int ncols = cols(time);
        vector[nrows] psi_bsld = link_function_inputs[,1];
        vector[nrows] psi_ks = link_function_inputs[,2];
        vector[nrows] psi_kg = link_function_inputs[,3];
        matrix[nrows, ncols] result;
        for (i in 1:ncols) {
            result[,i] = fmin(
                sld(time[,i], psi_bsld, psi_ks, psi_kg),
                8000.0
            );
        }
        return result;
    }
}
