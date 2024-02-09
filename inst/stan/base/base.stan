
functions {
{{ longitudinal.functions }}
{{ survival.functions }}
{{ link.data }}
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
{{ longitudinal.data }}
{{ link.data }}

{{ priors.data }}

}


transformed data {
{{ longitudinal.transformed_data }}
{{ survival.transformed_data }}
{{ link.transformed_data }}
}



parameters{
{{ longitudinal.parameters }}
{{ survival.parameters }}
{{ link.parameters }}
}



transformed parameters{
    //
    // Source - base/base.stan
    //

    // Log-likelihood values for using the loo package.
    vector[Nind] log_lik = rep_vector(0.0, Nind);

{{ longitudinal.transformed_parameters }}
{{ survival.transformed_parameters }}
{{ link.transformed_parameters }}

}


model{
{{ longitudinal.model }}
{{ survival.model }}
{{ link.model }}

{{ priors.model }}

    //
    // Source - base/base.stan
    //
    target += sum(log_lik);
}

generated quantities{

{{ longitudinal.generated_quantities }}
{{ survival.generated_quantities }}
{{ link.generated_quantities }}

}


