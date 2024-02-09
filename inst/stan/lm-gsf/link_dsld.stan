
functions {
    //
    // Source - lm-gsf/link_dsld.stan
    //

    // Derivative of SLD
    matrix link_dsld_contribution(
        matrix time,
        matrix link_function_inputs,
    ) {
        vector psi_bsld = link_function_inputs[1];
        vector psi_phi = link_function_inputs[2];
        vector psi_ks = link_function_inputs[3];
        vector psi_kg = link_function_inputs[4];
        int nrows = rows(psi_bsld);
        int ncols = cols(time);
        
        // Here we assume that psi's are replicated along the rows of the time matrix.
        matrix[nrows, ncols] psi_bsld_matrix = rep_matrix(psi_bsld, ncols);
        matrix[nrows, ncols] psi_ks_matrix = rep_matrix(psi_ks, ncols);
        matrix[nrows, ncols] psi_kg_matrix = rep_matrix(psi_kg, ncols);
        matrix[nrows, ncols] psi_phi_matrix = rep_matrix(psi_phi, ncols);
        
        matrix[nrows, ncols] result = fmin(
            8000.0,
            psi_bsld_matrix .* (
                (1 - psi_phi_matrix) .* psi_kg_matrix .* exp(psi_kg_matrix .* time) -
                psi_phi_matrix .* psi_ks_matrix .* exp(- psi_ks_matrix .* time)
            )
        );
        return result;
    }
}


