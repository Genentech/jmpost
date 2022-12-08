



parameters {
    // LongitudinalRandomSlope
    real lm_rs_intercept;
    real lm_rs_slope;
    real<lower=0.000000001> lm_rs_sigma;
    vector[Nind] lm_rs_rslope;
}


model {
    // LongitudinalRandomSlope
    vector[Nta_total] lm_rs_rslope_ind;
    for (i in 1:Nta_total) {
        lm_rs_rslope_ind[i] = lm_rs_rslope[ind_index[i]];
    }
    target += normal_lpdf(lm_rs_rslope | lm_rs_slope, lm_rs_sigma);
    vector[Nta_total] lm_rs_mu = lm_rs_intercept + lm_rs_rslope_ind .* Tobs;
    target += normal_lpdf(Yobs | lm_rs_mu, lm_rs_sigma);
    // Priors - TODO - Make user defined priors
    lm_rs_intercept ~ normal(30, 30);
    lm_rs_slope ~ normal(0, 10);
    lm_rs_sigma ~ cauchy(0, 2.5);
}

