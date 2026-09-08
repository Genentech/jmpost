functions {
    vector sld(vector time, vector baseline, vector shrinkage, vector growth) {
        return baseline .* exp(-shrinkage .* time) + growth .* time;
    }
}

data {
    int<lower=0> p_baseline_cov;
    matrix[n_subjects, p_baseline_cov] baseline_cov_design;
    int<lower=0> p_growth_cov;
    matrix[n_subjects, p_growth_cov] growth_cov_design;
}

transformed parameters {
    vector[n_subjects] baseline_location =
        rep_vector(log_mu_baseline, n_subjects) +
        baseline_cov_design * beta_baseline;
    vector[n_subjects] growth_location =
        rep_vector(log_mu_growth, n_subjects) +
        growth_cov_design * beta_growth;

    vector[n_tumour_all] Ypred = sld(
        tumour_time,
        baseline_idv[subject_tumour_index],
        shrinkage_idv[subject_tumour_index],
        growth_idv[subject_tumour_index]
    );

    long_obvs_log_lik[subject_tumour_index_obs] = vect_normal_log_dens(
        tumour_value[subject_tumour_index_obs],
        Ypred[subject_tumour_index_obs],
        rep_vector(sigma, n_tumour_obs)
    );
    if (n_tumour_cens > 0) {
        long_obvs_log_lik[subject_tumour_index_cens] = vect_normal_log_cum(
            tumour_value_lloq,
            Ypred[subject_tumour_index_cens],
            rep_vector(sigma, n_tumour_cens)
        );
    }
}

model {
    baseline_idv ~ lognormal(baseline_location, sigma_baseline);
    shrinkage_idv ~ lognormal(log(mu_shrinkage), sigma_shrinkage);
    growth_idv ~ lognormal(growth_location, sigma_growth);
}
