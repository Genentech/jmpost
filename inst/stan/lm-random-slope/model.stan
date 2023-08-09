



parameters {
    //
    // LongitudinalRandomSlope
    //
    real lm_rs_intercept;
    array [n_arms] real lm_rs_slope_mu;
    real<lower={{ machine_double_eps }}> lm_rs_slope_sigma;
    real<lower={{ machine_double_eps }}> lm_rs_sigma;
    vector[Nind] lm_rs_ind_rnd_slope;
}


transformed parameters {
    //
    // LongitudinalRandomSlope
    //
    for (i in 1:Nind) {
        log_lik[i] = normal_lpdf(
            lm_rs_ind_rnd_slope[i] |
            lm_rs_slope_mu[arm_index[i]],
            lm_rs_slope_sigma
        );
    }

    // log_lik += vect_normal_log_dens(
    //     lm_rs_ind_rnd_slope,
    //     to_vector(lm_rs_slope_mu[arm_index]),
    //     rep_vector(lm_rs_slope_sigma, Nind)
    // );

    vector[Nta_total] lm_rs_rslope_ind = to_vector(lm_rs_ind_rnd_slope[ind_index]);

    vector[Nta_total] Ypred = lm_rs_intercept + lm_rs_rslope_ind .* Tobs;

    log_lik += csr_matrix_times_vector(
        Nind,
        Nta_total,
        w_mat_inds_all_y,
        v_mat_inds_all_y,
        u_mat_inds_all_y,
        vect_normal_log_dens(
            Yobs,
            Ypred,
            rep_vector(lm_rs_sigma, Nta_total)
        )
    );
}

generated quantities {
    matrix[Nind, n_lm_time_grid] y_fit_at_time_grid;

    for (i in 1:Nind) {
        y_fit_at_time_grid[i] = lm_rs_intercept + lm_rs_ind_rnd_slope[i] .* to_row_vector(lm_time_grid);
    }
}

