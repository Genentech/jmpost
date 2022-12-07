

functions {
    // LinkRandomSlope
    matrix link_contribution(matrix time, matrix pars_lm) {
        vector[rows(pars_lm)] contrib = col(pars_lm, 1) .* col(pars_lm, 2);
        return  rep_matrix(contrib, cols(time));
    }
}



transformed parameters {
    // LinkRandomSlope
    matrix[Nind, 2] pars_lm = append_col(
        rep_matrix(link_lm_phi, Nind, 1),
        lm_rs_rslope
    );
}

parameters {
    // LinkRandomSlope
    real link_lm_phi;
}






