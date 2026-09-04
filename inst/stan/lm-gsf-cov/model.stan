data {
{% for parameter in ["mu_b", "omega_b", "mu_s", "omega_s", "mu_g", "omega_g", "mu_phi", "omega_phi"] -%}
    int<lower=0> p_lm_gsfc_{{ parameter }};
    matrix[n_subjects, p_lm_gsfc_{{ parameter }}] lm_gsfc_{{ parameter }}_design;
{% endfor -%}
}

transformed parameters {
    vector[n_subjects] lm_gsfc_ind_mu_b = {{ mu_b_predictor }};
    vector[n_subjects] lm_gsfc_ind_omega_b = safe_finite({{ omega_b_predictor }});
    vector[n_subjects] lm_gsfc_ind_mu_s = {{ mu_s_predictor }};
    vector[n_subjects] lm_gsfc_ind_omega_s = safe_finite({{ omega_s_predictor }});
    vector[n_subjects] lm_gsfc_ind_mu_g = {{ mu_g_predictor }};
    vector[n_subjects] lm_gsfc_ind_omega_g = safe_finite({{ omega_g_predictor }});
    vector[n_subjects] lm_gsfc_ind_mu_phi = {{ mu_phi_predictor }};
    vector[n_subjects] lm_gsfc_ind_omega_phi = safe_finite({{ omega_phi_predictor }});
{% if not centred_baseline -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsfc_psi_b = safe_positive(exp(lm_gsfc_ind_mu_b + lm_gsfc_eta_tilde_b .* lm_gsfc_ind_omega_b));
{%- endif %}
{% if not centred_shrinkage -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsfc_psi_s = safe_positive(exp(lm_gsfc_ind_mu_s + lm_gsfc_eta_tilde_s .* lm_gsfc_ind_omega_s));
{%- endif %}
{% if not centred_growth -%}
    vector<lower={{ machine_double_eps }}>[n_subjects] lm_gsfc_psi_g = safe_positive(exp(lm_gsfc_ind_mu_g + lm_gsfc_eta_tilde_g .* lm_gsfc_ind_omega_g));
{%- endif %}
{% if not centred_phi -%}
    vector[n_subjects] lm_gsfc_psi_phi_logit = lm_gsfc_ind_mu_phi + lm_gsfc_eta_tilde_phi .* lm_gsfc_ind_omega_phi;
{%- endif %}
    vector<lower={{ machine_double_eps }}, upper={{ 1 - machine_double_eps }}>[n_subjects] lm_gsfc_psi_phi = inv_logit(lm_gsfc_psi_phi_logit);
    vector[n_tumour_all] Ypred = sld(tumour_time, lm_gsfc_psi_b[subject_tumour_index], lm_gsfc_psi_s[subject_tumour_index], lm_gsfc_psi_g[subject_tumour_index], lm_gsfc_psi_phi[subject_tumour_index]);
    long_obvs_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(tumour_value[subject_tumour_index_obs], Ypred[subject_tumour_index_obs], {% if scaled_variance %} fmax(Ypred[subject_tumour_index_obs] * lm_gsfc_sigma, {{ machine_double_eps }}) {% else %} rep_vector(lm_gsfc_sigma, n_tumour_obs) {% endif %});
    if (n_tumour_cens > 0) long_obvs_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(tumour_value_lloq, Ypred[subject_tumour_index_cens], {% if scaled_variance %} fmax(Ypred[subject_tumour_index_cens] * lm_gsfc_sigma, {{ machine_double_eps }}) {% else %} rep_vector(lm_gsfc_sigma, n_tumour_cens) {% endif %});
}

model {
{% if centred_baseline -%}
    lm_gsfc_psi_b ~ lognormal(lm_gsfc_ind_mu_b, lm_gsfc_ind_omega_b);
{%- endif %}
{% if centred_shrinkage -%}
    lm_gsfc_psi_s ~ lognormal(lm_gsfc_ind_mu_s, lm_gsfc_ind_omega_s);
{%- endif %}
{% if centred_growth -%}
    lm_gsfc_psi_g ~ lognormal(lm_gsfc_ind_mu_g, lm_gsfc_ind_omega_g);
{%- endif %}
{% if centred_phi -%}
    lm_gsfc_psi_phi_logit ~ normal(lm_gsfc_ind_mu_phi, lm_gsfc_ind_omega_phi);
{%- endif %}
}
