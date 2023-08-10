
functions {
    // LinkRandomSlope
    matrix link_contribution(matrix time, matrix pars_lm) {
        return  rep_matrix(to_vector(pars_lm), cols(time));
    }
}

transformed parameters {
    // LinkRandomSlope
    matrix[Nind, 1] pars_lm = to_matrix(lm_rs_ind_rnd_slope .* link_lm_phi);
}

parameters {
    // LinkRandomSlope
    real link_lm_phi;
}
