

#
# TODO
#


functions {
    //
    // Source - lm-stein-fojo/link_ttg.stan
    //
    matrix link_ttg_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int nrows = rows(link_function_inputs);
        int ncols = cols(time);
        vector[nrows] psi_bsld = link_function_inputs[,1];
        vector[nrows] psi_ks = link_function_inputs[,2];
        vector[nrows] psi_kg = link_function_inputs[,3];
        vector[nrows] psi_phi = link_function_inputs[,4];
        vector[nrows] num = logit(psi_phi) + log(psi_ks ./ psi_kg);
        vector[nrows] denom = psi_ks + psi_kg;
        vector[nrows] ttg_contribution = num ./ denom;
        matrix[nrows, ncols] ttg_contribution_matrix = rep_matrix(ttg_contribution, ncols);
        return ttg_contribution_matrix;
    }
}

