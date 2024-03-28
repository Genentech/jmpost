
functions {
{{ longitudinal.functions }}
{{ survival.functions }}
{{ link.functions }}
}



data{
    //
    // Source - base/base.stan
    //
    int<lower=1> Nind;                 // Number of individuals.
    int<lower=1> n_studies;            // Number of studies.
    int<lower=1> n_arms;               // Number of treatment arms.
    array[Nind] int<lower=1,upper=n_studies> pt_study_index;  // Index of study per pt (PT index sorted)
    array[Nind] int<lower=1,upper=n_arms> pt_arm_index;       // Index of treatment arm per pt (PT index sorted)

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
    //
    // Source - base/base.stan
    //

    // Log-likelihood values for using the loo package.
    vector[Nind] log_lik = rep_vector(0.0, Nind);

{{ longitudinal.transformed_parameters }}
{{ link.transformed_parameters }}
{{ survival.transformed_parameters }}

}


model{
{{ longitudinal.model }}
{{ link.model }}
{{ survival.model }}

{{ priors.model }}

    //
    // Source - base/base.stan
    //
    target += sum(log_lik);
}

generated quantities{
{{ longitudinal.generated_quantities }}
{{ link.generated_quantities }}
{{ survival.generated_quantities }}
}


