



parameters{
    //
    // Source - lm-gsf/model.stan
    //

    vector[n_studies] lm_gsf_mu_bsld;
    vector[n_arms] lm_gsf_mu_ks;
    vector[n_arms] lm_gsf_mu_kg;
    vector[n_arms] lm_gsf_mu_phi;

    vector<lower={{ machine_double_eps }}>[n_studies] lm_gsf_omega_bsld;
    vector<lower={{ machine_double_eps }}>[n_arms] lm_gsf_omega_ks;
    vector<lower={{ machine_double_eps }}>[n_arms] lm_gsf_omega_kg;
    vector<lower={{ machine_double_eps }}>[n_arms] lm_gsf_omega_phi;

{% if centred -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_bsld;
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_ks;
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_kg;
    vector[n_subjects] lm_gsf_psi_phi_logit;
{% else -%}
    vector[n_subjects] lm_gsf_eta_tilde_bsld;
    vector[n_subjects] lm_gsf_eta_tilde_ks;
    vector[n_subjects] lm_gsf_eta_tilde_kg;
    vector[n_subjects] lm_gsf_eta_tilde_phi;
{%- endif -%}

    // Standard deviation of the error term
    real<lower={{ machine_double_eps }}> lm_gsf_sigma;

}





transformed parameters{
    //
    // Source - lm-gsf/model.stan
    //

{% if not centred -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_bsld = exp(
        lm_gsf_mu_bsld[subject_study_index] + (lm_gsf_eta_tilde_bsld .* lm_gsf_omega_bsld[subject_study_index])
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_ks = exp(
        lm_gsf_mu_ks[subject_arm_index] + (lm_gsf_eta_tilde_ks .* lm_gsf_omega_ks[subject_arm_index])
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsf_psi_kg = exp(
        lm_gsf_mu_kg[subject_arm_index] + (lm_gsf_eta_tilde_kg .* lm_gsf_omega_kg[subject_arm_index])
    );
    vector[n_subjects] lm_gsf_psi_phi_logit = (
        lm_gsf_mu_phi[subject_arm_index] + (lm_gsf_eta_tilde_phi .* lm_gsf_omega_phi[subject_arm_index])
    );
{%- endif -%}
    vector<
        lower={{ machine_double_eps }},
        upper={{ 1 - machine_double_eps }}
    >[n_subjects] lm_gsf_psi_phi = inv_logit(lm_gsf_psi_phi_logit);

    vector[n_tumour_all] Ypred;

    Ypred = sld(
        tumour_time,
        lm_gsf_psi_bsld[subject_tumour_index],
        lm_gsf_psi_ks[subject_tumour_index],
        lm_gsf_psi_kg[subject_tumour_index],
        lm_gsf_psi_phi[subject_tumour_index]
    );


    long_obvs_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        {%- if scaled_variance -%}
            Ypred[subject_tumour_index_obs] * lm_gsf_sigma
        {% else %}
            rep_vector(lm_gsf_sigma, n_tumour_obs)
        {%- endif -%}
    );
    if (n_tumour_cens > 0 ) {
        long_obvs_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
            tumour_value_lloq,
            Ypred[subject_tumour_index_cens],
            {%- if scaled_variance -%}
                Ypred[subject_tumour_index_cens] * lm_gsf_sigma
            {% else %}
                rep_vector(lm_gsf_sigma, n_tumour_cens)
            {%- endif -%} 
        );
    }
}


model {
    //
    // Source - lm-gsf/model.stan
    //
{% if centred %}
    lm_gsf_psi_bsld ~ lognormal(lm_gsf_mu_bsld[subject_study_index], lm_gsf_omega_bsld[subject_study_index]);
    lm_gsf_psi_ks ~ lognormal(lm_gsf_mu_ks[subject_arm_index], lm_gsf_omega_ks[subject_arm_index]);
    lm_gsf_psi_kg ~ lognormal(lm_gsf_mu_kg[subject_arm_index], lm_gsf_omega_kg[subject_arm_index]);
    lm_gsf_psi_phi_logit ~ normal(lm_gsf_mu_phi[subject_arm_index], lm_gsf_omega_phi[subject_arm_index]);
{%- endif -%}
}

