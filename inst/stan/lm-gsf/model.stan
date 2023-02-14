



functions {

    ////////////////
    //
    // GSF - lm-gsf/model.stan
    //
    //
    
    vector sld(
        vector time,
        vector psi_bsld,
        vector psi_ks,
        vector psi_kg,
        vector psi_phi
    ) {
        vector[num_elements(time)] psi_phi_mod = ifelse(
            is_negative(time),
            zeros_vector(num_elements(time)),
            psi_phi
        );
        vector[num_elements(time)] result = fmin(
            8000.0,
            psi_bsld .* (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
        );
        return result;
    }
}


parameters{
    
    ////////////////
    //
    // GSF - lm-gsf/model.stan
    //
    //

    // Population parameters.
    vector<lower=0>[n_studies] mu_bsld;
    vector<lower=0>[n_arms] mu_ks;
    vector<lower=0>[n_arms] mu_kg;
    vector<lower=0, upper=1>[n_arms] mu_phi;

    real<lower=0> omega_bsld;
    real<lower=0> omega_ks;
    real<lower=0> omega_kg;
    real<lower=0> omega_phi;

    // Standard deviation for RUV.
    real<lower=0.00001, upper=100> sigma;

    // Random effects.
    vector[Nind] eta_tilde_bsld;
    vector[Nind] eta_tilde_ks;
    vector[Nind] eta_tilde_kg;
    vector[Nind] eta_tilde_phi;

}




transformed parameters{
    
    ////////////////
    //
    // GSF - lm-gsf/model.stan
    //
    //
    
    // Non-centered reparametrization for hierarchical models.
    vector[Nind] psi_bsld = exp(log(mu_bsld[study_index]) + eta_tilde_bsld * omega_bsld);
    vector[Nind] psi_ks = exp(log(mu_ks[arm_index]) + eta_tilde_ks * omega_ks);
    vector[Nind] psi_kg = exp(log(mu_kg[arm_index]) + eta_tilde_kg * omega_kg);
    vector[Nind] psi_phi =  inv_logit(logit(mu_phi[arm_index]) + eta_tilde_phi * omega_phi);

    vector[Nta_total] Ypred;

    Ypred = sld(
        Tobs,
        psi_bsld[ind_index], psi_ks[ind_index], psi_kg[ind_index], psi_phi[ind_index]
    );
    
    // Reverse implementation from Rstan helper function
    log_lik += csr_matrix_times_vector(
        Nind,
        Nta_obs_y,
        w_mat_inds_obs_y,
        v_mat_inds_obs_y,
        u_mat_inds_obs_y,
        vect_normal_log_dens(
            Yobs[obs_y_index], Ypred[obs_y_index], Ypred[obs_y_index] * sigma
        )
    );
    
    log_lik += csr_matrix_times_vector(
        Nind,
        Nta_cens_y,
        w_mat_inds_cens_y,
        v_mat_inds_cens_y,
        u_mat_inds_cens_y,
        vect_normal_log_cum(
            Ythreshold, Ypred[cens_y_index], Ypred[cens_y_index] * sigma
        )
    );
}


model{
    
    ////////////////
    //
    // GSF - lm-gsf/model.stan
    //
    //

    // Priors definition.
    mu_bsld ~ lognormal( log(55), 5);
    mu_ks ~ lognormal(  0, 0.5);
    mu_kg ~ lognormal(-0.36,1);
    mu_phi ~ beta(2,8);

    omega_bsld ~ lognormal(0,1);
    omega_ks ~ lognormal(0,1);
    omega_kg ~ lognormal(0,1);
    omega_phi ~ lognormal(0,1);

    sigma ~ lognormal(-1.6,0.8);

    eta_tilde_bsld ~ normal(0,5);
    eta_tilde_ks ~ normal(0,5);
    eta_tilde_kg ~ normal(0,5);
    eta_tilde_phi ~ normal(0,5);
}




