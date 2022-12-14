

functions {
    // LinkRandomSlope
    matrix link_contribution(matrix time, matrix pars_lm) {
        return  rep_matrix(to_vector(pars_lm), cols(time));
    }
}

transformed parameters {
    // LinkRandomSlope
    matrix[Nind, 1] pars_lm = to_matrix(lm_rs_rslope .* link_lm_phi);
}

parameters {
    // LinkRandomSlope
    real link_lm_phi;
}

model {
    link_lm_phi ~ normal(0.2, 0.5);
}




