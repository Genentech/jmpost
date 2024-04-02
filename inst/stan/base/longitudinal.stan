
functions {
    {{ stan.functions }}
}


data{
    //
    // Source - base/longitudinal.stan
    //

    // Longitudinal data
    int<lower=1> Nta_total;            // Total number of tumour assessments.
    int<lower=1> Nta_obs_y;            // Number of observed tumour assessments (not censored).
    int<lower=0> Nta_cens_y;           // Number of censored tumour assessments (below threshold).

    array[Nta_total] int ind_index;          // Index of individuals for each tumour assessment.
    array[Nta_obs_y] int obs_y_index;        // Index of observed tumour assessments (not censored).
    array[Nta_cens_y] int cens_y_index;      // Index of censored tumour assessments.

    vector[Nta_total] Yobs;   // Array of individual responses.
    vector[Nta_total] Tobs;   // Individual timepoints.
    real Ythreshold;          // Censoring threshold.

    // Matrix of individuals x observed tumour assessments (sparse matrix of 0s and 1s),
    // so the dimension is n_subjects x Nta_obs_y.
    array [3] int<lower=1> n_mat_inds_obs_y;
    vector[n_mat_inds_obs_y[1]] w_mat_inds_obs_y;
    array[n_mat_inds_obs_y[2]] int v_mat_inds_obs_y;
    array[n_mat_inds_obs_y[3]] int u_mat_inds_obs_y;

    // Matrix of individuals x censored tumour assessments (sparse matrix of 0s and 1s).
    // so the dimension is n_subjects x Nta_cens_y.
    array [3] int<lower=0> n_mat_inds_cens_y;
    vector[n_mat_inds_cens_y[1]] w_mat_inds_cens_y;
    array[n_mat_inds_cens_y[2]] int v_mat_inds_cens_y;
    array[n_mat_inds_cens_y[3]] int u_mat_inds_cens_y;

    // Matrix of all individuals x tumour assessments (sparse matrix of 0s and 1s).
    // so the dimension is n_subjects x Nta_total.
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
    vector[Nta_total] Ypred_log_lik = rep_vector(0, Nta_total);

    {{ stan.transformed_parameters }}

    //
    // Source - base/longitudinal.stan
    //
    log_lik += csr_matrix_times_vector(
        n_subjects,
        Nta_total,
        w_mat_inds_all_y,
        v_mat_inds_all_y,
        u_mat_inds_all_y,
        Ypred_log_lik
    );
}

model {
    {{ stan.model }}
}

generated quantities {
    {{ stan.generated_quantities }}

    //
    // Source - base/longitudinal.stan
    //
    matrix[n_pt_select_index, n_lm_time_grid] y_fit_at_time_grid;
    if (n_lm_time_grid > 0) {
        for (i in 1:n_pt_select_index) {
            int current_pt_index = pt_select_index[i];
            y_fit_at_time_grid[i, ] = lm_predict_individual_patient(
                lm_time_grid,
                long_gq_parameters[current_pt_index, ]
            );
        }
    }
}
