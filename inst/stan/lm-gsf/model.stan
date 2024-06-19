



parameters{
    //
    // Source - lm-gsf/model.stan
    //

    vector[n_studies] lm_gsf_mu_bsld;
    vector[n_arms] lm_gsf_mu_ks;
    vector[n_arms] lm_gsf_mu_kg;

    real<lower={{ machine_double_eps }}> lm_gsf_omega_bsld;
    real<lower={{ machine_double_eps }}> lm_gsf_omega_ks;
    real<lower={{ machine_double_eps }}> lm_gsf_omega_kg;

{% if centred -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_bsld;
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_ks;
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_kg;
{% else -%}
    vector[n_subjects] lm_gsf_eta_tilde_bsld;
    vector[n_subjects] lm_gsf_eta_tilde_ks;
    vector[n_subjects] lm_gsf_eta_tilde_kg;
{%- endif -%}

    // Phi Parameters
    vector<lower={{ machine_double_eps }}, upper={{ 1 - machine_double_eps }}>[n_subjects] lm_gsf_psi_phi;
    vector<lower={{ machine_double_eps }}>[n_arms] lm_gsf_a_phi;
    vector<lower={{ machine_double_eps }}>[n_arms] lm_gsf_b_phi;

    // Standard deviation of the error term
    real<lower={{ machine_double_eps }}> lm_gsf_sigma;

}





transformed parameters{
    //
    // Source - lm-gsf/model.stan
    //

{% if not centred -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_bsld = exp(
        lm_gsf_mu_bsld[subject_study_index] + (lm_gsf_eta_tilde_bsld * lm_gsf_omega_bsld)
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_ks = exp(
        lm_gsf_mu_ks[subject_arm_index] + (lm_gsf_eta_tilde_ks * lm_gsf_omega_ks)
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_kg = exp(
        lm_gsf_mu_kg[subject_arm_index] + (lm_gsf_eta_tilde_kg * lm_gsf_omega_kg)
    );
{%- endif -%}

    vector[n_tumour_all] Ypred;

    Ypred = sld(
        tumour_time,
        lm_gsf_psi_bsld[subject_tumour_index],
        lm_gsf_psi_ks[subject_tumour_index],
        lm_gsf_psi_kg[subject_tumour_index],
        lm_gsf_psi_phi[subject_tumour_index]
    );


    Ypred_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs] * lm_gsf_sigma
    );
    if (n_tumour_cens > 0 ) {
        Ypred_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
            tumour_value_lloq,
            Ypred[subject_tumour_index_cens],
            Ypred[subject_tumour_index_cens] * lm_gsf_sigma
        );
    }
}


model {
    //
    // Source - lm-gsf/model.stan
    //
{% if centred %}
    lm_gsf_psi_bsld ~ lognormal(lm_gsf_mu_bsld[subject_study_index], lm_gsf_omega_bsld);
    lm_gsf_psi_ks ~ lognormal(lm_gsf_mu_ks[subject_arm_index], lm_gsf_omega_ks);
    lm_gsf_psi_kg ~ lognormal(lm_gsf_mu_kg[subject_arm_index], lm_gsf_omega_kg);
{%- endif -%}
    lm_gsf_psi_phi ~ beta(lm_gsf_a_phi[subject_arm_index], lm_gsf_b_phi[subject_arm_index]);
}

