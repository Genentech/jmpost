
functions {
    //
    // Source - lm-gsf/link_dsld.stan
    //

    // Derivative of SLD
    matrix dtsld(
        matrix time,
        row_vector psi_bsld,
        row_vector psi_ks,
        row_vector psi_kg,
        row_vector psi_phi
    ) {
        // Here we assume that psi's are replicated along the rows of the time matrix.
        matrix[rows(time), cols(psi_bsld)] psi_bsld_matrix = rep_matrix(psi_bsld, rows(time));
        matrix[rows(time), cols(psi_ks)] psi_ks_matrix = rep_matrix(psi_ks, rows(time));
        matrix[rows(time), cols(psi_kg)] psi_kg_matrix = rep_matrix(psi_kg, rows(time));
        // We also assume that all the time values are positive. Therefore no need to change phi.
        matrix[rows(time), cols(psi_phi)] psi_phi_matrix = rep_matrix(psi_phi, rows(time));
        matrix[rows(time), cols(time)] result = fmin(
            8000.0,
            psi_bsld_matrix .* (
                (1 - psi_phi_matrix) .* psi_kg_matrix .* exp(psi_kg_matrix .* time) -
                psi_phi_matrix .* psi_ks_matrix .* exp(- psi_ks_matrix .* time)
            )
        );
        return result;
    }
}


