data {
    int<lower=0> p_lm_rsc_mu;
    matrix[n_subjects, p_lm_rsc_mu] lm_rsc_mu_design;
    int<lower=0> p_lm_rsc_slope_mu;
    matrix[n_subjects, p_lm_rsc_slope_mu] lm_rsc_slope_mu_design;
    int<lower=0> p_lm_rsc_slope_sigma;
    matrix[n_subjects, p_lm_rsc_slope_sigma] lm_rsc_slope_sigma_design;
}

transformed parameters {
    // Subject-specific distribution parameters.
    vector[n_subjects] lm_rsc_ind_intercept = {{ mu_predictor }};
    vector[n_subjects] lm_rsc_ind_slope_mu = {{ slope_mu_predictor }};
    vector[n_subjects] lm_rsc_ind_slope_sigma = {{ slope_sigma_predictor }};

    vector[n_tumour_all] lm_rsc_rslope_ind =
        to_vector(lm_rsc_ind_rnd_slope[subject_tumour_index]);
    vector[n_tumour_all] Ypred =
        lm_rsc_ind_intercept[subject_tumour_index] +
        lm_rsc_rslope_ind .* tumour_time;

    long_obvs_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        {%- if scaled_variance -%}
            fmax(Ypred[subject_tumour_index_obs] * lm_rsc_sigma, {{ machine_double_eps }})
        {% else %}
            rep_vector(lm_rsc_sigma, n_tumour_obs)
        {% endif -%}
    );
    if (n_tumour_cens > 0) {
        long_obvs_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
            tumour_value_lloq,
            Ypred[subject_tumour_index_cens],
            {%- if scaled_variance -%}
                fmax(Ypred[subject_tumour_index_cens] * lm_rsc_sigma, {{ machine_double_eps }})
            {% else %}
                rep_vector(lm_rsc_sigma, n_tumour_cens)
            {% endif -%}
        );
    }
}

model {
    lm_rsc_ind_rnd_slope ~ normal(
        lm_rsc_ind_slope_mu,
        lm_rsc_ind_slope_sigma
    );
}
