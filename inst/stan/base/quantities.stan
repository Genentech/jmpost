
data {
    //
    // Source - base/generated_quantities_data.stan
    //
    int <lower=1> gq_n_quant;

{% if not include_gq_longitudinal_pop -%}
    array[gq_n_quant] int<lower=1, upper=n_subjects> gq_pt_index;
{%- endif %}

    vector[gq_n_quant] gq_times;

    array [gq_n_quant] int <lower=1, upper=n_arms> gq_long_pop_arm_index;
    array [gq_n_quant] int <lower=1, upper=n_studies> gq_long_pop_study_index;
}



generated quantities {
    //
    // Source - base/generated_quantities.stan - Longitudinal
    //
{% if include_gq_longitudinal_idv or include_gq_longitudinal_pop -%}
    vector[gq_n_quant] y_fit_at_time_grid;
    {

    {% if include_gq_longitudinal_idv -%}
    y_fit_at_time_grid = lm_predict_individual_patient(
        gq_times,
        long_gq_parameters[gq_pt_index, ]
    );
    {%- endif %}

    {% if include_gq_longitudinal_pop -%}
    y_fit_at_time_grid = lm_predict_individual_patient(
        gq_times,
        long_gq_pop_parameters
    );
    {%- endif %}

    }
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
        link_function_inputs[gq_pt_index, ],
        link_coefficients,
        nodes,
        weights,
        os_cov_contribution[gq_pt_index]
    );

    log_haz_fit_at_time_grid = log_hazard(
        rep_matrix(gq_times, 1),
        pars_os,
        link_function_inputs[gq_pt_index, ],
        link_coefficients,
        os_cov_contribution[gq_pt_index]
    )[, 1];
{%- endif %}
}
