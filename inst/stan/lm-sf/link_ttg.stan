
functions {
    //
    // Source - lm-sf/link_ttg.stan
    //
    matrix link_ttg_contribution(
        matrix time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg
    ) {
        int nrows = rows(psi_bsld);
        int ncols = cols(time);
        vector[nrows] num = log(psi_ks) - log(psi_kg);
        vector[nrows] denom = psi_ks + psi_kg;
        vector[nrows] ttg_contribution = num ./ denom;
        matrix[nrows, ncols] ttg_contribution_matrix = rep_matrix(ttg_contribution, ncols);
        return ttg_contribution_matrix;
    }
}

