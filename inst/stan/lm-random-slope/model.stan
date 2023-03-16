



parameters {
    //
    // LongitudinalRandomSlope
    //
    real lm_rs_intercept;
    array [n_arms] real lm_rs_slope_mu;
    real<lower=0.000000001> lm_rs_slope_sigma;
    real<lower=0.000000001> lm_rs_sigma;
    vector[Nind] lm_rs_rslope;
}


model {
    //
    // LongitudinalRandomSlope
    //
    vector[Nta_total] lm_rs_rslope_ind;
    for (i in 1:Nta_total) {
        lm_rs_rslope_ind[i] = lm_rs_rslope[ind_index[i]];
    }
    target += normal_lpdf(lm_rs_rslope | lm_rs_slope_mu[arm_index], lm_rs_slope_sigma);
    vector[Nta_total] lm_rs_mu = lm_rs_intercept + lm_rs_rslope_ind .* Tobs;
    target += normal_lpdf(Yobs | lm_rs_mu, lm_rs_sigma);
}

