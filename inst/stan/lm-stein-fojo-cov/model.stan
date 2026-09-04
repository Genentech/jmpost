data {
{% for parameter in ["mu_b", "omega_b", "mu_s", "omega_s", "mu_g", "omega_g"] -%}
    int<lower=0> p_lm_sfc_{{ parameter }};
    matrix[n_subjects, p_lm_sfc_{{ parameter }}] lm_sfc_{{ parameter }}_design;
{% endfor -%}
}

transformed parameters {
    vector[n_subjects] lm_sfc_ind_mu_b = {{ mu_b_predictor }};
    vector[n_subjects] lm_sfc_ind_omega_b = {{ omega_b_predictor }};
    vector[n_subjects] lm_sfc_ind_mu_s = {{ mu_s_predictor }};
    vector[n_subjects] lm_sfc_ind_omega_s = {{ omega_s_predictor }};
    vector[n_subjects] lm_sfc_ind_mu_g = {{ mu_g_predictor }};
    vector[n_subjects] lm_sfc_ind_omega_g = {{ omega_g_predictor }};

    vector<lower={{ machine_double_eps }}>[n_subjects] lm_sfc_psi_b = exp(
        lm_sfc_ind_mu_b + lm_sfc_eta_tilde_b .* lm_sfc_ind_omega_b
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_sfc_psi_s = exp(
        lm_sfc_ind_mu_s + lm_sfc_eta_tilde_s .* lm_sfc_ind_omega_s
    );
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_sfc_psi_g = exp(
        lm_sfc_ind_mu_g + lm_sfc_eta_tilde_g .* lm_sfc_ind_omega_g
    );

    vector[n_tumour_all] Ypred = sld(
        tumour_time,
        lm_sfc_psi_b[subject_tumour_index],
        lm_sfc_psi_s[subject_tumour_index],
        lm_sfc_psi_g[subject_tumour_index]
    );

    long_obvs_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        {%- if scaled_variance -%}
            fmax(Ypred[subject_tumour_index_obs] * lm_sfc_sigma, {{ machine_double_eps }})
        {% else %}
            rep_vector(lm_sfc_sigma, n_tumour_obs)
        {%- endif -%}
    );
    if (n_tumour_cens > 0) {
        long_obvs_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
            tumour_value_lloq,
            Ypred[subject_tumour_index_cens],
            {%- if scaled_variance -%}
                fmax(Ypred[subject_tumour_index_cens] * lm_sfc_sigma, {{ machine_double_eps }})
            {% else %}
                rep_vector(lm_sfc_sigma, n_tumour_cens)
            {%- endif -%}
        );
    }
}
