



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


transformed parameters {
    //
    // LongitudinalRandomSlope
    //
    
    log_lik += vect_normal_log_dens(
        lm_rs_rslope,
        to_vector(lm_rs_slope_mu[arm_index]),
        rep_vector(lm_rs_slope_sigma, Nind)
    );
    
    vector[Nta_total] lm_rs_rslope_ind  = lm_rs_rslope[ind_index[1:Nta_total]];
    
    log_lik += csr_matrix_times_vector(
        Nind,
        Nta_total,
        w_mat_inds_all_y,
        v_mat_inds_all_y,
        u_mat_inds_all_y,
        vect_normal_log_dens(
            Yobs,
            lm_rs_intercept + lm_rs_rslope_ind .* Tobs,
            rep_vector(lm_rs_sigma, Nta_total)
        )
    );
}
