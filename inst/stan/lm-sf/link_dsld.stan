
functions {
    //
    // Source - lm-sf/link_dsld.stan
    //

    // Derivative of SLD
    matrix link_dsld_contribution(
        matrix time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg
    ) {
        int nrows = rows(psi_bsld);
        int ncols = cols(time);
        
        // Here we assume that psi's are replicated along the rows of the time matrix.
        matrix[nrows, ncols] psi_bsld_matrix = rep_matrix(psi_bsld, ncols);
        matrix[nrows, ncols] psi_ks_matrix = rep_matrix(psi_ks, ncols);
        matrix[nrows, ncols] psi_kg_matrix = rep_matrix(psi_kg, ncols);
        
        matrix[nrows, ncols] result = fmin(
            8000.0,
            psi_bsld_matrix .* (
                psi_kg_matrix .* exp(psi_kg_matrix .* time) -
                psi_ks_matrix .* exp(- psi_ks_matrix .* time)
            )
        );
        return result;
    }
}


