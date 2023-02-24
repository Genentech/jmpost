

functions {
    //
    // Source - lm-gsf/link_ttg.stan
    //
    matrix ttg(
        matrix time,
        row_vector psi_bsld,
        row_vector psi_ks,
        row_vector psi_kg,
        row_vector psi_phi
    ) {
        row_vector[num_elements(psi_ks)] num = logit(psi_phi) + log(psi_ks ./ psi_kg);
        row_vector[num_elements(psi_ks)] denom = psi_ks + psi_kg;
        row_vector[num_elements(psi_ks)] ttg_contribution = num ./ denom;
        matrix[rows(time), cols(time)] ttg_contribution_matrix = rep_matrix(ttg_contribution, rows(time));
        return ttg_contribution_matrix;
    }
}

