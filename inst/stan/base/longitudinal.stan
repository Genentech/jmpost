
functions {
{{ stan.functions }}
}


data{
    //
    // Source - base/longitudinal.stan
    //

    // Longitudinal data
    int<lower=1> n_tumour_all;            // Total number of tumour assessments.
    int<lower=1> n_tumour_obs;            // Number of observed tumour assessments (not censored).
    int<lower=0> n_tumour_cens;           // Number of censored tumour assessments (below threshold).

    array[n_tumour_all] int subject_tumour_index;          // Index of individuals for each tumour assessment.
    array[n_tumour_obs] int subject_tumour_index_obs;        // Index of observed tumour assessments (not censored).
    array[n_tumour_cens] int subject_tumour_index_cens;      // Index of censored tumour assessments.

    vector[n_tumour_all] tumour_value;   // Array of individual responses.
    vector[n_tumour_all] tumour_time;   // Individual timepoints.
    real tumour_value_lloq;          // Censoring threshold.

    // Matrix of individuals x observed tumour assessments (sparse matrix of 0s and 1s),
    // so the dimension is n_subjects x n_tumour_obs.
    array [3] int<lower=1> n_mat_inds_obs_y;
    vector[n_mat_inds_obs_y[1]] w_mat_inds_obs_y;
    array[n_mat_inds_obs_y[2]] int v_mat_inds_obs_y;
    array[n_mat_inds_obs_y[3]] int u_mat_inds_obs_y;

    // Matrix of individuals x censored tumour assessments (sparse matrix of 0s and 1s).
    // so the dimension is n_subjects x n_tumour_cens.
    array [3] int<lower=0> n_mat_inds_cens_y;
    vector[n_mat_inds_cens_y[1]] w_mat_inds_cens_y;
    array[n_mat_inds_cens_y[2]] int v_mat_inds_cens_y;
    array[n_mat_inds_cens_y[3]] int u_mat_inds_cens_y;

    // Matrix of all individuals x tumour assessments (sparse matrix of 0s and 1s).
    // so the dimension is n_subjects x n_tumour_all.
    array [3] int<lower=0> n_mat_inds_all_y;
    vector[n_mat_inds_all_y[1]] w_mat_inds_all_y;
    array[n_mat_inds_all_y[2]] int v_mat_inds_all_y;
    array[n_mat_inds_all_y[3]] int u_mat_inds_all_y;

{{ stan.data }}
}

transformed data {
{{ stan.transformed_data }}
}

parameters {
{{ stan.parameters }}
}

transformed parameters {
    //
    // Source - base/longitudinal.stan
    //
    // Vector to store per observation log-likelihood values
    vector[n_tumour_all] long_obvs_log_lik = rep_vector(0, n_tumour_all);

    {{ stan.transformed_parameters }}
}

model {
{{ stan.model }}
    //
    // Source - base/longitudinal.stan
    //
    target += sum(long_obvs_log_lik);
}

generated quantities {
{{ stan.generated_quantities }}
}
