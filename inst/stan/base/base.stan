
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
{{ priors.parameters }}
}



transformed parameters{
{{ priors.transformed_parameters }}
{{ longitudinal.transformed_parameters }}
{{ link.transformed_parameters }}
{{ survival.transformed_parameters }}

    //
    // Source - base/base.stan
    //
    {% if has_long_submodel -%}
        vector[n_subjects] long_subj_log_lik = rep_vector(0, n_subjects);
        for (i in 1:n_tumour_all) {
            long_subj_log_lik[subject_tumour_index[i]] += long_obvs_log_lik[i];
        }
    {%- endif %}
    {% if has_os_submodel and has_long_submodel -%}
        vector[n_subjects] log_lik = os_subj_log_lik + long_subj_log_lik;
    {% else if has_os_submodel -%}
        vector[n_subjects] log_lik = os_subj_log_lik;
    {% else if has_long_submodel and not has_os_submodel -%}
        vector[n_subjects] log_lik = long_subj_log_lik;
    {%- endif -%}
}


model{
{{ longitudinal.model }}
{{ link.model }}
{{ survival.model }}
{{ priors.model }}
}

generated quantities{
    // Joint prior on the declared parameter scale, including truncation terms.
    real lprior = 0;
    {
{{ log_prior }}
    }
{{ longitudinal.generated_quantities }}
{{ link.generated_quantities }}
{{ survival.generated_quantities }}
{{ priors.generated_quantities }}
}
