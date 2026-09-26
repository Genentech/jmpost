functions {
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return (
            long_gq_parameters[, 1] + long_gq_parameters[, 2] .* time
        );
    }
}

generated quantities {
{% if include_gq_longitudinal_idv -%}
    matrix[n_subjects, 2] long_gq_parameters;
    long_gq_parameters[, 1] = lm_rsc_ind_intercept;
    long_gq_parameters[, 2] = lm_rsc_ind_rnd_slope;
{%- endif %}

{% if include_gq_longitudinal_pop -%}
    matrix[gq_n_quant, 2] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = {{ mu_population_predictor }};
    long_gq_pop_parameters[, 2] = {{ slope_mu_population_predictor }};
{%- endif %}
}
