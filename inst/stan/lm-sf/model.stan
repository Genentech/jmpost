



parameters{
    //
    // Source - lm-sf/model.stan
    //

    // Population parameters.
    vector<lower=0.0000001>[n_studies] lm_sf_mu_bsld;
    vector<lower=0.0000001>[n_arms] lm_sf_mu_ks;
    vector<lower=0.0000001>[n_arms] lm_sf_mu_kg;
    
    real<lower=0> lm_sf_omega_bsld;
    real<lower=0> lm_sf_omega_ks;
    real<lower=0> lm_sf_omega_kg;

    // Standard deviation for RUV.
    real<lower=0.00001, upper=100> lm_sf_sigma;

    // Random effects.
    vector[Nind] lm_sf_eta_tilde_bsld;
    vector[Nind] lm_sf_eta_tilde_ks;
    vector[Nind] lm_sf_eta_tilde_kg;
}





transformed parameters{
    //
    // Source - lm-sf/model.stan
    //

    // Non-centered reparametrization for hierarchical models.
    vector[Nind] lm_sf_psi_bsld = exp(
        log(lm_sf_mu_bsld[study_index]) + lm_sf_eta_tilde_bsld * lm_sf_omega_bsld
    );

    vector[Nind] lm_sf_psi_ks = exp(
        log(lm_sf_mu_ks[arm_index]) + lm_sf_eta_tilde_ks * lm_sf_omega_ks
    );

    vector[Nind] lm_sf_psi_kg = exp(
        log(lm_sf_mu_kg[arm_index]) + lm_sf_eta_tilde_kg * lm_sf_omega_kg
    );

    vector[Nta_total] Ypred;

    Ypred = sld(
        Tobs,
        lm_sf_psi_bsld[ind_index],
        lm_sf_psi_ks[ind_index],
        lm_sf_psi_kg[ind_index]
    );

    // Reverse implementation from Rstan helper function
    vector[Nind] log_lik_obs = csr_matrix_times_vector(
        Nind,
        Nta_obs_y,
        w_mat_inds_obs_y,
        v_mat_inds_obs_y,
        u_mat_inds_obs_y,
        vect_normal_log_dens(
            Yobs[obs_y_index],
            Ypred[obs_y_index],
            Ypred[obs_y_index] * lm_sf_sigma
        )
    );
    log_lik += log_lik_obs;

    if (Nta_cens_y > 0 ) {
        
        print("---Ythreshold---");
        print(Ythreshold);
        print("---Ypred[cens_y_index]---");
        print(Ypred[cens_y_index]);
        print("---lm_sf_sigma---");
        print(lm_sf_sigma);
        print("---Nta_cens_y---");
        print(Nta_cens_y);
        vector[Nind] log_lik_cens = csr_matrix_times_vector(
            Nind,
            Nta_cens_y,
            w_mat_inds_cens_y,
            v_mat_inds_cens_y,
            u_mat_inds_cens_y,
            vect_normal_log_cum(
                Ythreshold,
                Ypred[cens_y_index],
                Ypred[cens_y_index] * lm_sf_sigma
            )
        );
        log_lik += log_lik_cens;
    }
}

generated quantities {
    matrix[Nind, n_lm_time_grid] y_fit_at_time_grid;
    vector[n_lm_time_grid] rep_i;

    for (i in 1:Nind) {
        y_fit_at_time_grid[i] = to_row_vector(sld(
            lm_time_grid,
            rep_vector(lm_sf_psi_bsld[i], n_lm_time_grid),
            rep_vector(lm_sf_psi_ks[i], n_lm_time_grid),
            rep_vector(lm_sf_psi_kg[i], n_lm_time_grid)
        ));
    }
}


