
row_vector beta_dt(matrix time, row_vector psi_bsld, row_vector psi_ks, row_vector psi_kg, row_vector psi_phi){

    matrix[rows(time), cols(time)] beta_dt = dtsld(time, psi_bsld, psi_ks, psi_kg, psi_phi);
    return beta_dt;
}
