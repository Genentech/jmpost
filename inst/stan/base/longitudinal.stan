

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
    // so the dimension is Nind x Nta_obs_y.
    array [3] int<lower=1> n_mat_inds_obs_y;
    vector[n_mat_inds_obs_y[1]] w_mat_inds_obs_y;
    array[n_mat_inds_obs_y[2]] int v_mat_inds_obs_y;
    array[n_mat_inds_obs_y[3]] int u_mat_inds_obs_y;

    // Matrix of individuals x censored tumour assessments (sparse matrix of 0s and 1s).
    // so the dimension is Nind x Nta_cens_y.
    array [3] int<lower=0> n_mat_inds_cens_y;
    vector[n_mat_inds_cens_y[1]] w_mat_inds_cens_y;
    array[n_mat_inds_cens_y[2]] int v_mat_inds_cens_y;
    array[n_mat_inds_cens_y[3]] int u_mat_inds_cens_y;

    // Matrix of all individuals x tumour assessments (sparse matrix of 0s and 1s).
    // so the dimension is Nind x Nta_total.
    array [3] int<lower=0> n_mat_inds_all_y;
    vector[n_mat_inds_all_y[1]] w_mat_inds_all_y;
    array[n_mat_inds_all_y[2]] int v_mat_inds_all_y;
    array[n_mat_inds_all_y[3]] int u_mat_inds_all_y;
}

