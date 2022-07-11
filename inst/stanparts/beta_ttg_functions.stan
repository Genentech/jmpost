row_vector beta_ttg(matrix time, row_vector psi_ks, row_vector psi_kg, row_vector psi_phi){

    row_vector[cols(time)] ttg_contribution = ttg(psi_ks, psi_kg, psi_phi);\n
    matrix[rows(time), cols(time)] beta_ttg = rep_matrix(ttg_contribution, rows(time));
    return beta_ttg;
}
