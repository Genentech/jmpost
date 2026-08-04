
functions {
    // Expected tumour size value
    vector sld(vector tumour_time, vector baseline, vector shrinkage, vector growth) {
        vector[rows(tumour_time)] tumour_value;
        tumour_value = baseline .* exp(- shrinkage .* tumour_time)  +
                       growth .* tumour_time;
        return tumour_value;
    }
}

transformed parameters{

    // Calculated the fitted Tumour values
    vector[n_tumour_all] Ypred = sld(
        tumour_time,
        baseline_idv[subject_tumour_index],
        shrinkage_idv[subject_tumour_index],
        growth_idv[subject_tumour_index]
    );

    // Calculate per observation log-likelihood for {loo} integration
    // These values are automatically added to the target for you
    long_obvs_log_lik = vect_normal_log_dens(
        tumour_value,
        Ypred,
        rep_vector(sigma, n_tumour_all) // broadcast sigma to the length of Ypred
    );
}

model {
    // Define the heirarchical relationship between the individual
    // and population level parameters
    baseline_idv ~ lognormal(log(mu_baseline), sigma_baseline);
    shrinkage_idv ~ lognormal(log(mu_shrinkage), sigma_shrinkage);
    growth_idv ~ lognormal(log(mu_growth), sigma_growth);
}
