
data {
    //
    // Source - base/generated_quantities_data.stan
    //
    int <lower=1> gq_n_quant;

{% if include_gq_survival_idv or include_gq_longitudinal_idv -%}
    array[gq_n_quant] int<lower=1, upper=n_subjects> gq_subject_index;
{%- endif %}


{% if include_gq_survival_pred -%}
    int<lower=0> gq_n_par;
    matrix[gq_n_quant, gq_n_par] gq_link_function_inputs;
    matrix[gq_n_quant, p_os_cov_design] gq_os_cov_design;
{%- endif %}

    vector[gq_n_quant] gq_times;

    array [gq_n_quant] int <lower=1, upper=n_arms> gq_long_pop_arm_index;
    array [gq_n_quant] int <lower=1, upper=n_studies> gq_long_pop_study_index;
}



generated quantities {
    //
    // Source - base/generated_quantities.stan - Longitudinal
    //
{% if include_gq_longitudinal_idv -%}
    vector[gq_n_quant] y_fit_at_time_grid = lm_predict_value(
        gq_times,
        long_gq_parameters[gq_subject_index, ]
    );
{%- endif %}

{% if include_gq_longitudinal_pop -%}
    vector[gq_n_quant] y_fit_at_time_grid = lm_predict_value(
        gq_times,
        long_gq_pop_parameters
    );
{%- endif %}


    //
    // Source - base/generated_quantities.stan - Survival
    //
{% if include_gq_survival_idv -%}
    vector[gq_n_quant] log_surv_fit_at_time_grid;
    vector[gq_n_quant] log_haz_fit_at_time_grid;

    log_surv_fit_at_time_grid = log_survival(
        gq_times,
        pars_os,
        link_function_inputs[gq_subject_index, ],
        link_coefficients,
        nodes,
        weights,
        os_cov_contribution[gq_subject_index]
    );

    log_haz_fit_at_time_grid = log_hazard(
        rep_matrix(gq_times, 1),
        pars_os,
        link_function_inputs[gq_subject_index, ],
        link_coefficients,
        os_cov_contribution[gq_subject_index]
    )[, 1];
{%- endif %}


{% if include_gq_survival_pred -%}
    vector[gq_n_quant] log_surv_fit_at_time_grid;
    vector[gq_n_quant] log_haz_fit_at_time_grid;

    log_surv_fit_at_time_grid = log_survival(
        gq_times,
        pars_os,
        gq_link_function_inputs,
        link_coefficients,
        nodes,
        weights,
        get_os_cov_contribution(gq_os_cov_design, beta_os_cov)
    );

    log_haz_fit_at_time_grid = log_hazard(
        rep_matrix(gq_times, 1),
        pars_os,
        gq_link_function_inputs,
        link_coefficients,
        get_os_cov_contribution(gq_os_cov_design, beta_os_cov)
    )[, 1];
{%- endif %}
}
