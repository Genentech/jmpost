



parameters {
    //
    // Source - lm-random-slope/model.stan
    //

    array [n_studies] real lm_rs_intercept;
    array [n_arms] real lm_rs_slope_mu;
    real<lower={{ machine_double_eps }}> lm_rs_slope_sigma;
    real<lower={{ machine_double_eps }}> lm_rs_sigma;
    vector[Nind] lm_rs_ind_rnd_slope;
}


transformed parameters {
    //
    // Source - lm-random-slope/model.stan
    //
    vector[Nind] lm_rs_ind_intercept = to_vector(lm_rs_intercept[pt_study_index]);
    vector[Nta_total] lm_rs_rslope_ind = to_vector(lm_rs_ind_rnd_slope[ind_index]);

    vector[Nta_total] Ypred = lm_rs_ind_intercept[ind_index] + lm_rs_rslope_ind .* Tobs;

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


model {
    //
    // Source - lm-random-slope/model.stan
    //

    lm_rs_ind_rnd_slope ~ normal(
        lm_rs_slope_mu[pt_arm_index],
        lm_rs_slope_sigma
    );
}


generated quantities {
    //
    // Source - lm-random-slope/model.stan
    //
    matrix[n_pt_select_index, n_lm_time_grid] y_fit_at_time_grid;
    if (n_lm_time_grid > 0) {
        for (i in 1:n_pt_select_index) {
            int current_pt_index = pt_select_index[i];
            y_fit_at_time_grid[i, ] =
                lm_rs_ind_intercept[current_pt_index] +
                lm_rs_ind_rnd_slope[current_pt_index] .*
                to_row_vector(lm_time_grid);
        }
    }
}
