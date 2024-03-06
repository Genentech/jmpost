
functions {
    //
    // Source - lm-gsf/link_dsld.stan
    //

    // Derivative of SLD
    matrix link_dsld_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int nrows = rows(link_function_inputs);
        int ncols = cols(time);
        vector[nrows] psi_bsld = link_function_inputs[,1];
        vector[nrows] psi_ks = link_function_inputs[,2];
        vector[nrows] psi_kg = link_function_inputs[,3];
        vector[nrows] psi_phi = link_function_inputs[,4];
        
        // Here we assume that psi's are replicated along the rows of the time matrix.
        matrix[nrows, ncols] psi_bsld_matrix = rep_matrix(psi_bsld, ncols);
        matrix[nrows, ncols] psi_ks_matrix = rep_matrix(psi_ks, ncols);
        matrix[nrows, ncols] psi_kg_matrix = rep_matrix(psi_kg, ncols);
        matrix[nrows, ncols] psi_phi_matrix = rep_matrix(psi_phi, ncols);

        psi_phi_matrix = replace_lt_0(psi_phi_matrix, 0);

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


