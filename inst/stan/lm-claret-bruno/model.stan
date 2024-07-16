



parameters{
    //
    // Source - lm-claret-bruno/model.stan
    //

    vector[n_studies] lm_clbr_mu_b;
    vector[n_arms] lm_clbr_mu_g;
    vector[n_arms] lm_clbr_mu_c;
    vector[n_arms] lm_clbr_mu_p;

    real<lower={{ machine_double_eps }}> lm_clbr_omega_b;
    real<lower={{ machine_double_eps }}> lm_clbr_omega_g;
    real<lower={{ machine_double_eps }}> lm_clbr_omega_c;
    real<lower={{ machine_double_eps }}> lm_clbr_omega_p;

{% if centred -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_b;
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_g;
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_c;
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_p;
{% else -%}
    vector[n_subjects] lm_clbr_eta_b;
    vector[n_subjects] lm_clbr_eta_g;
    vector[n_subjects] lm_clbr_eta_c;
    vector[n_subjects] lm_clbr_eta_p;
{%- endif -%}

    // Standard deviation of the error term
    real<lower={{ machine_double_eps }}> lm_clbr_sigma;

}





transformed parameters{
    //
    // Source - lm-claret-bruno/model.stan
    //

{% if not centred -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_b = exp(
        lm_clbr_mu_b[subject_study_index] + (lm_clbr_eta_b * lm_clbr_omega_b)
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_g = exp(
        lm_clbr_mu_g[subject_arm_index] + (lm_clbr_eta_g * lm_clbr_omega_g)
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_c = exp(
        lm_clbr_mu_c[subject_arm_index] + (lm_clbr_eta_c * lm_clbr_omega_c)
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbr_ind_p = exp(
        lm_clbr_mu_p[subject_arm_index] + (lm_clbr_eta_p * lm_clbr_omega_p)
    );
{%- endif -%}

    vector[n_tumour_all] Ypred;

    Ypred = sld(
        tumour_time,
        lm_clbr_ind_b[subject_tumour_index],
        lm_clbr_ind_g[subject_tumour_index],
        lm_clbr_ind_c[subject_tumour_index],
        lm_clbr_ind_p[subject_tumour_index]
    );


    Ypred_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs] * lm_clbr_sigma
    );
    if (n_tumour_cens > 0 ) {
        Ypred_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
            tumour_value_lloq,
            Ypred[subject_tumour_index_cens],
            Ypred[subject_tumour_index_cens] * lm_clbr_sigma
        );
    }
}


model {
    //
    // Source - lm-claret-bruno/model.stan
    //
{% if centred %}
    lm_clbr_ind_b ~ lognormal(lm_clbr_mu_b[subject_study_index], lm_clbr_omega_b);
    lm_clbr_ind_g ~ lognormal(lm_clbr_mu_g[subject_arm_index], lm_clbr_omega_g);
    lm_clbr_ind_c ~ lognormal(lm_clbr_mu_c[subject_arm_index], lm_clbr_omega_c);
    lm_clbr_ind_p ~ lognormal(lm_clbr_mu_p[subject_arm_index], lm_clbr_omega_p);
{%- endif -%}

}
