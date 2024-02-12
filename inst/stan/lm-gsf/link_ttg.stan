

functions {
    //
    // Source - lm-gsf/link_ttg.stan
    //
    matrix link_ttg_contrib(
        matrix time,
        matrix link_function_inputs,
    ) {
        vector psi_bsld = link_function_inputs[1];
        vector psi_phi = link_function_inputs[2];
        vector psi_ks = link_function_inputs[3];
        vector psi_kg = link_function_inputs[4];
        int nrows = rows(psi_bsld);
        int ncols = cols(time);
        vector[nrows] num = logit(psi_phi) + log(psi_ks ./ psi_kg);
        vector[nrows] denom = psi_ks + psi_kg;
        vector[nrows] ttg_contribution = num ./ denom;
        matrix[nrows, ncols] ttg_contribution_matrix = rep_matrix(ttg_contribution, ncols);
        return ttg_contribution_matrix;
    }
}

