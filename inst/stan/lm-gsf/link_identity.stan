
functions {
    //
    // Source - lm-gsf/link_identity.stan
    //

    // Identity of SLD
    matrix link_identity_contribution(
        matrix time,
        matrix link_function_inputs,
    ) {
        vector psi_bsld = link_function_inputs[1];
        vector psi_phi = link_function_inputs[2];
        vector psi_ks = link_function_inputs[3];
        vector psi_kg = link_function_inputs[4];
        int nrows = rows(psi_bsld);
        int ncols = cols(time);
        matrix[nrows, ncols] result;
        for (i in 1:ncols) {
            result[,i] = fmin(
                sld(time[,i], psi_bsld, psi_ks, psi_kg, psi_phi),
                8000.0
            );
        }
        return result;
    }
}


