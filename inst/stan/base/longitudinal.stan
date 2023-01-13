

data{

    // Longditudinal data
    int<lower=1> Nta_total;            // Total number of tumor assessments.
    // int<lower=1> Nta_obs_y;            // Number of observed tumor assessments (not censored).
    // int<lower=1> Nta_cens_y;           // Number of censored tumor assessments (below threshold).

    array[Nta_total] int ind_index;          // Index of individuals for each tumor assessment.
    // array[Nta_obs_y] int obs_y_index;        // Index of observed tumor assessments (not censored).
    // array[Nta_cens_y] int cens_y_index;      // Index of censored tumor assessments.

    // int<lower=1> n_studies;                                // Number of studies.
    // array[Nind] int<lower=1,upper=n_studies> study_index;  // Index of study for all individuals.
    // int<lower=1> n_arms;                                   // Number of treatment arms.
    // array[Nind] int<lower=1,upper=n_arms> arm_index;       // Index of treatment arm for all individuals.


    // TODO - Need to get a better understanding of this shared
    // int<lower=1> n_sld_par_shared;
    // array[n_sld_par_shared] int<lower=1,upper=n_arms> sld_par_shared;
    // int<lower=1> n_sld_par_separate;
    // array[n_sld_par_separate] int<lower=1,upper=n_arms> sld_par_separate;


    // array[n_arms] int<lower=1,upper=n_studies> arm_to_study_index;


    // Ragged index vector of individuals per treatment arm (see R code).
    // array[n_arms] int<lower=1,upper=Nind> n_index_per_arm;
    // array[Nind] int<lower=1,upper=Nind> index_per_arm;


    vector[Nta_total] Yobs;   // Array of individual responses.
    vector[Nta_total] Tobs;   // Individual timepoints.
    // real Ythreshold;              // Censoring threshold.


    // // Matrix of individuals x observed tumor assessments (sparse matrix of 0s and 1s),
    // // so the dimension is Nind x Nta_obs_y.
    // int<lower=1> n_w_mat_inds_obs_y;
    // vector[n_w_mat_inds_obs_y] w_mat_inds_obs_y;
    // int<lower=1> n_v_mat_inds_obs_y;
    // array[n_v_mat_inds_obs_y] int v_mat_inds_obs_y;
    // int<lower=1> n_u_mat_inds_obs_y;
    // array[n_u_mat_inds_obs_y] int u_mat_inds_obs_y;


    // // Matrix of individuals x censored tumor assessments (sparse matrix of 0s and 1s).
    // // so the dimension is Nind x Nta_cens_y.
    // int<lower=1> n_w_mat_inds_cens_y;
    // vector[n_w_mat_inds_cens_y] w_mat_inds_cens_y;
    // int<lower=1> n_v_mat_inds_cens_y;
    // array[n_v_mat_inds_cens_y] int v_mat_inds_cens_y;
    // int<lower=1> n_u_mat_inds_cens_y;
    // array[n_u_mat_inds_cens_y] int u_mat_inds_cens_y;

}

