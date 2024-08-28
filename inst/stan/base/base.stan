
functions {
{{ longitudinal.functions }}
{{ survival.functions }}
{{ link.functions }}
}



data{
    //
    // Source - base/base.stan
    //
    int<lower=1> n_subjects;           // Number of individuals.
    int<lower=1> n_studies;            // Number of studies.
    int<lower=1> n_arms;               // Number of treatment arms.
    // Index of study per subject (sorted by subject factor level)
    array[n_subjects] int<lower=1,upper=n_studies> subject_study_index;
    // Index of treatment arm per subject (sorted by subject factor level)
    array[n_subjects] int<lower=1,upper=n_arms> subject_arm_index;

{{ survival.data }}
{{ link.data }}
{{ longitudinal.data }}
{{ priors.data }}
}


transformed data {
{{ longitudinal.transformed_data }}
{{ link.transformed_data }}
{{ survival.transformed_data }}
}



parameters{
{{ longitudinal.parameters }}
{{ link.parameters }}
{{ survival.parameters }}
}



transformed parameters{
{{ longitudinal.transformed_parameters }}
{{ link.transformed_parameters }}
{{ survival.transformed_parameters }}

    //
    // Source - base/base.stan
    //
    {% if has_os_submodel and not has_long_submodel -%}
        vector[n_subjects] log_lik = os_subj_log_lik;
    {% else if has_long_submodel and not has_os_submodel -%}
        vector[n_tumour_all] log_lik = long_obvs_log_lik;
    {%- endif -%}
}


model{
{{ longitudinal.model }}
{{ link.model }}
{{ survival.model }}
{{ priors.model }}
}

generated quantities{
{{ longitudinal.generated_quantities }}
{{ link.generated_quantities }}
{{ survival.generated_quantities }}
}


