
functions {
    //
    // Source - base/base.stan
    //

{% if link_none %}
    // If user has requested link_none then provide a dummy link_contribution function
    // that does nothing
    matrix link_contribution(matrix time, matrix pars_lm) {
        return  rep_matrix(0, rows(time), cols(time));
    }
{% endif %}

{{ longitudinal.functions }}
{{ survival.functions }}

}



data{
    //
    // Source - base/base.stan
    //
    int<lower=1> Nind;                 // Number of individuals.
    int<lower=1> n_studies;            // Number of studies.
    int<lower=1> n_arms;               // Number of treatment arms.
    array[Nind] int<lower=1,upper=n_studies> study_index;  // Index of study for all individuals.
    array[Nind] int<lower=1,upper=n_arms> arm_index;       // Index of treatment arm for all individuals.
    array[n_arms] int<lower=1,upper=n_studies> arm_to_study_index;

    // Ragged index vector of individuals per treatment arm (see R code).
    array[n_arms] int<lower=1,upper=Nind> n_index_per_arm;
    array[Nind] int<lower=1,upper=Nind> index_per_arm;


{{ survival.data }}
{{ longitudinal.data }}

}


transformed data {

{{ longitudinal.transformed_data }}
{{ survival.transformed_data }}


{% if link_none %}
    //
    // Source - base/base.stan
    //

    // If user has requested link_none then provide a dummy pars_lm object
    // that contains nothing
    matrix[Nind, 0] pars_lm = rep_matrix(0, Nind, 0);
{% endif %}

}



parameters{
{{ longitudinal.parameters }}
{{ survival.parameters }}
}



transformed parameters{
    //
    // Source - base/base.stan
    //

    // Log-likelihood values for using the loo package.
    vector[Nind] log_lik = rep_vector(0.0, Nind);
{{ longitudinal.transformed_parameters }}
{{ survival.transformed_parameters }}
}


model{
{{ longitudinal.model }}
{{ survival.model }}
{{ priors.model }}

    //
    // Source - base/base.stan
    //
    target += sum(log_lik);
}

generated quantities{

{{ longitudinal.generated_quantities }}
{{ survival.generated_quantities }}

}


