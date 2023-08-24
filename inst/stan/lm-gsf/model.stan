



parameters{
    //
    // Source - lm-gsf/model.stan
    //

    // Population parameters.
    vector<lower={{ machine_double_eps }}>[n_studies] lm_gsf_mu_bsld;
    vector<lower={{ machine_double_eps }}>[n_arms] lm_gsf_mu_ks;
    vector<lower={{ machine_double_eps }}>[n_arms] lm_gsf_mu_kg;
    vector<lower={{ machine_double_eps }}, upper={{ 1 - machine_double_eps }}>[n_arms] lm_gsf_mu_phi;

    real<lower={{ machine_double_eps }}> lm_gsf_omega_bsld;
    real<lower={{ machine_double_eps }}> lm_gsf_omega_ks;
    real<lower={{ machine_double_eps }}> lm_gsf_omega_kg;
    real<lower={{ machine_double_eps }}> lm_gsf_omega_phi;

    // Standard deviation for RUV.
    real<lower={{ machine_double_eps }}> lm_gsf_sigma;

    // Random effects.
    vector[Nind] lm_gsf_eta_tilde_bsld;
    vector[Nind] lm_gsf_eta_tilde_ks;
    vector[Nind] lm_gsf_eta_tilde_kg;
    vector[Nind] lm_gsf_eta_tilde_phi;

}





transformed parameters{
    //
    // Source - lm-gsf/model.stan
    //

    // Non-centered reparametrization for hierarchical models.
    vector[Nind] lm_gsf_psi_bsld = exp(
        log(lm_gsf_mu_bsld[pt_study_index]) + lm_gsf_eta_tilde_bsld * lm_gsf_omega_bsld
    );

    vector[Nind] lm_gsf_psi_ks = exp(
        log(lm_gsf_mu_ks[pt_arm_index]) + lm_gsf_eta_tilde_ks * lm_gsf_omega_ks
    );

    vector[Nind] lm_gsf_psi_kg = exp(
        log(lm_gsf_mu_kg[pt_arm_index]) + lm_gsf_eta_tilde_kg * lm_gsf_omega_kg
    );

    vector[Nind] lm_gsf_psi_phi =  inv_logit(
        logit(lm_gsf_mu_phi[pt_arm_index]) + lm_gsf_eta_tilde_phi * lm_gsf_omega_phi
    );

    vector[Nta_total] Ypred;

    Ypred = sld(
        Tobs,
        lm_gsf_psi_bsld[ind_index],
        lm_gsf_psi_ks[ind_index],
        lm_gsf_psi_kg[ind_index],
        lm_gsf_psi_phi[ind_index]
    );


    // Reverse implementation from Rstan helper function
    log_lik += csr_matrix_times_vector(
        Nind,
        Nta_obs_y,
        w_mat_inds_obs_y,
        v_mat_inds_obs_y,
        u_mat_inds_obs_y,
        vect_normal_log_dens(
            Yobs[obs_y_index],
            Ypred[obs_y_index],
            Ypred[obs_y_index] * lm_gsf_sigma
        )
    );

    if (Nta_cens_y > 0 ) {
        log_lik += csr_matrix_times_vector(
            Nind,
            Nta_cens_y,
            w_mat_inds_cens_y,
            v_mat_inds_cens_y,
            u_mat_inds_cens_y,
            vect_normal_log_cum(
                Ythreshold,
                Ypred[cens_y_index],
                Ypred[cens_y_index] * lm_gsf_sigma
            )
        );
    }
}


generated quantities {
    //
    // Source - lm-gsf/model.stan
    //
    matrix[n_pt_select_index, n_lm_time_grid] y_fit_at_time_grid;
    if (n_lm_time_grid > 0) {
        for (i in 1:n_pt_select_index) {
            int current_pt_index = pt_select_index[i];
            y_fit_at_time_grid[i, ] = to_row_vector(sld(
                lm_time_grid,
                rep_vector(lm_gsf_psi_bsld[current_pt_index], n_lm_time_grid),
                rep_vector(lm_gsf_psi_ks[current_pt_index], n_lm_time_grid),
                rep_vector(lm_gsf_psi_kg[current_pt_index], n_lm_time_grid),
                rep_vector(lm_gsf_psi_phi[current_pt_index], n_lm_time_grid)
            ));
        }
    }
}
