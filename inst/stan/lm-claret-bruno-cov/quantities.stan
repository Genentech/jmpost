functions {
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return sld(time, long_gq_parameters[, 1], long_gq_parameters[, 2], long_gq_parameters[, 3], long_gq_parameters[, 4]);
    }
}
generated quantities {
{% if include_gq_longitudinal_idv -%}
    matrix[n_subjects, 4] long_gq_parameters;
    long_gq_parameters[, 1] = lm_clbrc_psi_b;
    long_gq_parameters[, 2] = lm_clbrc_psi_g;
    long_gq_parameters[, 3] = lm_clbrc_psi_c;
    long_gq_parameters[, 4] = lm_clbrc_psi_p;
{%- endif %}
{% if include_gq_longitudinal_pop -%}
    matrix[gq_n_quant, 4] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = exp({{ mu_b_population_predictor }});
    long_gq_pop_parameters[, 2] = exp({{ mu_g_population_predictor }});
    long_gq_pop_parameters[, 3] = exp({{ mu_c_population_predictor }});
    long_gq_pop_parameters[, 4] = exp({{ mu_p_population_predictor }});
{%- endif %}
}
