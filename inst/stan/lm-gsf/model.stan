



functions {


    //- sld ----
    // SLD model (GSF)
    // @class LM - GSF
    row_vector sld(
        row_vector time,
        row_vector psi_bsld,
        row_vector psi_ks,
        row_vector psi_kg,
        row_vector psi_phi
    ) {
        row_vector[num_elements(time)] psi_phi_mod = ifelse(
            is_negative(time),
            zeros_row_vector(num_elements(time)),
            psi_phi
        );
        row_vector[num_elements(time)] result = fmin(
            8000.0,
            psi_bsld .* (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
        );
        return result;
    }


    //- ttg ----
    // Time-to-growth (TTG)
    // @class LM - GSF
    matrix ttg(
        matrix time,
        row_vector psi_bsld,
        row_vector psi_ks,
        row_vector psi_kg,
        row_vector psi_phi
    ) {
        row_vector[num_elements(psi_ks)] num = logit(psi_phi) + log(psi_ks ./ psi_kg);
        row_vector[num_elements(psi_ks)] denom = psi_ks + psi_kg;
        row_vector[num_elements(psi_ks)] ttg_contribution = num ./ denom;
        matrix[rows(time), cols(time)] ttg_contribution_matrix = rep_matrix(ttg_contribution, rows(time));
        return ttg_contribution_matrix;
    }


    //- dtsld ----
    // Derivative of SLD
    // @class LM - GSF
    matrix dtsld(
        matrix time,
        row_vector psi_bsld,
        row_vector psi_ks,
        row_vector psi_kg,
        row_vector psi_phi
    ) {
        // Here we assume that psi's are replicated along the rows of the time matrix.
        matrix[rows(time), cols(psi_bsld)] psi_bsld_matrix = rep_matrix(psi_bsld, rows(time));
        matrix[rows(time), cols(psi_ks)] psi_ks_matrix = rep_matrix(psi_ks, rows(time));
        matrix[rows(time), cols(psi_kg)] psi_kg_matrix = rep_matrix(psi_kg, rows(time));
        // We also assume that all the time values are positive. Therefore no need to change phi.
        matrix[rows(time), cols(psi_phi)] psi_phi_matrix = rep_matrix(psi_phi, rows(time));
        matrix[rows(time), cols(time)] result = fmin(
            8000.0,
            psi_bsld_matrix .* (
                (1 - psi_phi_matrix) .* psi_kg_matrix .* exp(psi_kg_matrix .* time) -
                psi_phi_matrix .* psi_ks_matrix .* exp(- psi_ks_matrix .* time)
            )
        );
        return result;
    }

}




parameters{
    
    // Hyper parameters
    real<lower=0, upper=100> mean_mu_ks;
    real<lower=0, upper=100> mean_mu_kg;
    real<lower=0.001, upper=0.999> mean_mu_phi;
    real<lower=0, upper=10> sd_mu_ks;
    real<lower=0, upper=10> sd_mu_kg;
    real<lower=0, upper=10> sd_mu_phi;

    // Population parameters.
    row_vector<lower=0>[n_studies] mu_bsld;
    row_vector<lower=0>[n_arms] mu_ks;
    row_vector<lower=0>[n_arms] mu_kg;
    row_vector<lower=0, upper=1>[n_arms] mu_phi;

    real<lower=0> omega_bsld;
    real<lower=0> omega_ks;
    real<lower=0> omega_kg;
    real<lower=0> omega_phi;

    // Standard deviation for RUV.
    real<lower=0.00001, upper=100> sigma;

    // Random effects.
    row_vector[Nind] eta_tilde_bsld;
    row_vector[Nind] eta_tilde_ks;
    row_vector[Nind] eta_tilde_kg;
    row_vector[Nind] eta_tilde_phi;

}




transformed parameters{
    // Non-centered reparametrization for hierarchical models.
    row_vector[Nind] psi_bsld = exp(log(mu_bsld[study_index]) + eta_tilde_bsld * omega_bsld);
    row_vector[Nind] psi_ks = exp(log(mu_ks[arm_index]) + eta_tilde_ks * omega_ks);
    row_vector[Nind] psi_kg = exp(log(mu_kg[arm_index]) + eta_tilde_kg * omega_kg);
    row_vector[Nind] psi_phi =  inv_logit(logit(mu_phi[arm_index]) + eta_tilde_phi * omega_phi);

    row_vector[Nta_total] Ypred;

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
    real ypred_ij;
    real yobs_ij;
    
    // Hyper priors definition.
    mean_mu_ks ~ lognormal(1,0.5); // log(3)
    mean_mu_kg ~ lognormal(-0.36,1); // log(0.7)
    mean_mu_phi ~ beta(5,5);
    sd_mu_ks ~ lognormal(0,0.5);
    sd_mu_kg ~ lognormal(0,0.5);
    sd_mu_phi ~ lognormal(0,0.5);

    // Priors definition.
    mu_bsld ~ lognormal(55,5);

    mu_ks[sld_par_shared] ~ lognormal(mean_mu_ks, sd_mu_ks);
    mu_kg[sld_par_shared] ~ lognormal(mean_mu_kg, sd_mu_kg);
    logit(mu_phi[sld_par_shared]) ~ normal(logit(mean_mu_phi), sd_mu_phi);

    mu_ks[sld_par_separate] ~ lognormal(1,0.5);
    mu_kg[sld_par_separate] ~ lognormal(-0.36,1);
    mu_phi[sld_par_separate] ~ beta(5,5);

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




