



parameters{
    //
    // Source - lm-stein-fojo/model.stan
    //

    vector[n_studies] lm_sf_mu_bsld;
    vector[n_arms] lm_sf_mu_ks;
    vector[n_arms] lm_sf_mu_kg;

    real<lower={{ machine_double_eps }}> lm_sf_omega_bsld;
    real<lower={{ machine_double_eps }}> lm_sf_omega_ks;
    real<lower={{ machine_double_eps }}> lm_sf_omega_kg;

{% if centred -%}
    vector<lower={{ machine_double_eps }}>[Nind] lm_sf_psi_bsld;
    vector<lower={{ machine_double_eps }}>[Nind] lm_sf_psi_ks;
    vector<lower={{ machine_double_eps }}>[Nind] lm_sf_psi_kg;
{% else -%}
    vector[Nind] lm_sf_eta_tilde_bsld;
    vector[Nind] lm_sf_eta_tilde_ks;
    vector[Nind] lm_sf_eta_tilde_kg;
{%- endif -%}

    // Standard deviation of the error term
    real<lower={{ machine_double_eps }}> lm_sf_sigma;

}





transformed parameters{
    //
    // Source - lm-stein-fojo/model.stan
    //

{% if not centred -%}
    vector<lower={{ machine_double_eps }}>[Nind] lm_sf_psi_bsld = exp(
        lm_sf_mu_bsld[pt_study_index] + (lm_sf_eta_tilde_bsld * lm_sf_omega_bsld)
    );
    vector<lower={{ machine_double_eps }}>[Nind] lm_sf_psi_ks = exp(
        lm_sf_mu_ks[pt_arm_index] + (lm_sf_eta_tilde_ks * lm_sf_omega_ks)
    );
    vector<lower={{ machine_double_eps }}>[Nind] lm_sf_psi_kg = exp(
        lm_sf_mu_kg[pt_arm_index] + (lm_sf_eta_tilde_kg * lm_sf_omega_kg)
    );
{%- endif -%}

    vector[Nta_total] Ypred;

    Ypred = sld(
        Tobs,
        lm_sf_psi_bsld[ind_index],
        lm_sf_psi_ks[ind_index],
        lm_sf_psi_kg[ind_index]
    );

    Ypred_log_lik[obs_y_index] = vect_normal_log_dens(
        Yobs[obs_y_index],
        Ypred[obs_y_index],
        Ypred[obs_y_index] * lm_sf_sigma
    );
    if (Nta_cens_y > 0 ) {
        Ypred_log_lik[cens_y_index] = vect_normal_log_cum(
            Ythreshold,
            Ypred[cens_y_index],
            Ypred[cens_y_index] * lm_sf_sigma
        );
    }
}


model {
    //
    // Source - lm-stein-fojo/model.stan
    //
{% if centred %}
    lm_sf_psi_bsld ~ lognormal(lm_sf_mu_bsld[pt_study_index], lm_sf_omega_bsld);
    lm_sf_psi_ks ~ lognormal(lm_sf_mu_ks[pt_arm_index], lm_sf_omega_ks);
    lm_sf_psi_kg ~ lognormal(lm_sf_mu_kg[pt_arm_index], lm_sf_omega_kg);
{%- endif -%}
}

generated quantities {
    //
    // Source - lm-stein-fojo/model.stan
    //
    matrix[Nind, 3] long_gq_parameters;
    long_gq_parameters[, 1] = lm_gsf_psi_bsld;
    long_gq_parameters[, 2] = lm_gsf_psi_ks;
    long_gq_parameters[, 3] = lm_gsf_psi_kg;
}
