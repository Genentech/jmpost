data {
{% for parameter in ["mu_b", "omega_b", "mu_g", "omega_g", "mu_c", "omega_c", "mu_p", "omega_p"] -%}
    int<lower=0> p_lm_clbrc_{{ parameter }};
    matrix[n_subjects, p_lm_clbrc_{{ parameter }}] lm_clbrc_{{ parameter }}_design;
{% endfor -%}
}

transformed parameters {
    vector[n_subjects] lm_clbrc_ind_mu_b = {{ mu_b_predictor }};
    vector[n_subjects] lm_clbrc_ind_omega_b = safe_finite({{ omega_b_predictor }});
    vector[n_subjects] lm_clbrc_ind_mu_g = {{ mu_g_predictor }};
    vector[n_subjects] lm_clbrc_ind_omega_g = safe_finite({{ omega_g_predictor }});
    vector[n_subjects] lm_clbrc_ind_mu_c = {{ mu_c_predictor }};
    vector[n_subjects] lm_clbrc_ind_omega_c = safe_finite({{ omega_c_predictor }});
    vector[n_subjects] lm_clbrc_ind_mu_p = {{ mu_p_predictor }};
    vector[n_subjects] lm_clbrc_ind_omega_p = safe_finite({{ omega_p_predictor }});
{% if not centred_baseline -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbrc_psi_b = safe_positive(exp(lm_clbrc_ind_mu_b + lm_clbrc_eta_tilde_b .* lm_clbrc_ind_omega_b));
{%- endif %}
{% if not centred_growth -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbrc_psi_g = safe_positive(exp(lm_clbrc_ind_mu_g + lm_clbrc_eta_tilde_g .* lm_clbrc_ind_omega_g));
{%- endif %}
{% if not centred_resistance -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbrc_psi_c = safe_positive(exp(lm_clbrc_ind_mu_c + lm_clbrc_eta_tilde_c .* lm_clbrc_ind_omega_c));
{%- endif %}
{% if not centred_inhibition -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_clbrc_psi_p = safe_positive(exp(lm_clbrc_ind_mu_p + lm_clbrc_eta_tilde_p .* lm_clbrc_ind_omega_p));
{%- endif %}
    vector[n_tumour_all] Ypred = sld(tumour_time, lm_clbrc_psi_b[subject_tumour_index], lm_clbrc_psi_g[subject_tumour_index], lm_clbrc_psi_c[subject_tumour_index], lm_clbrc_psi_p[subject_tumour_index]);
    long_obvs_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(tumour_value[subject_tumour_index_obs], Ypred[subject_tumour_index_obs], {% if scaled_variance %} fmax(Ypred[subject_tumour_index_obs] * lm_clbrc_sigma, {{ machine_double_eps }}) {% else %} rep_vector(lm_clbrc_sigma, n_tumour_obs) {% endif %});
    if (n_tumour_cens > 0) long_obvs_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(tumour_value_lloq, Ypred[subject_tumour_index_cens], {% if scaled_variance %} fmax(Ypred[subject_tumour_index_cens] * lm_clbrc_sigma, {{ machine_double_eps }}) {% else %} rep_vector(lm_clbrc_sigma, n_tumour_cens) {% endif %});
}

model {
{% if centred_baseline -%}
    lm_clbrc_psi_b ~ lognormal(lm_clbrc_ind_mu_b, lm_clbrc_ind_omega_b);
{%- endif %}
{% if centred_growth -%}
    lm_clbrc_psi_g ~ lognormal(lm_clbrc_ind_mu_g, lm_clbrc_ind_omega_g);
{%- endif %}
{% if centred_resistance -%}
    lm_clbrc_psi_c ~ lognormal(lm_clbrc_ind_mu_c, lm_clbrc_ind_omega_c);
{%- endif %}
{% if centred_inhibition -%}
    lm_clbrc_psi_p ~ lognormal(lm_clbrc_ind_mu_p, lm_clbrc_ind_omega_p);
{%- endif %}
}
