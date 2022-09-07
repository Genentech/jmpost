int<lower=1> Nind; // Number of individuals.
  int<lower=1> Nta_total; // Total number of tumor assessments.
  int<lower=1> Nta_obs_y; // Number of observed tumor assessments (not censored).
  int<lower=1> Nta_cens_y; // Number of censored tumor assessments (below threshold).
  int<lower=1> Nind_dead; // Number of dead individuals (observed survival time).

  int ind_index[Nta_total]; // Index of individuals for each tumor assessment.
  int obs_y_index[Nta_obs_y]; // Index of observed tumor assessments (not censored).
  int cens_y_index[Nta_cens_y]; // Index of censored tumor assessments.
  int dead_ind_index[Nind_dead]; // Index of dead individuals (observed survival time).

  int<lower=1> n_studies; // Number of studies.
  int<lower=1,upper=n_studies> study_index[Nind]; // Index of study for all individuals.
  int<lower=1> n_arms; // Number of treatment arms.
  int<lower=1,upper=n_arms> arm_index[Nind]; // Index of treatment arm for all individuals.

  int<lower=1,upper=Nind> n_save_individual;
  int<lower=1,upper=Nind> index_save_individual[n_save_individual];

  int<lower=1> n_sld_par_shared;
  int<lower=1,upper=n_arms> sld_par_shared[n_sld_par_shared];
  int<lower=1> n_sld_par_separate;
  int<lower=1,upper=n_arms> sld_par_separate[n_sld_par_separate];

  int<lower=1,upper=n_studies> arm_to_study_index[n_arms];

  // Ragged index vector of individuals per treatment arm (see R code).
  int<lower=1,upper=Nind> n_index_per_arm[n_arms];
  int<lower=1,upper=Nind> index_per_arm[Nind];

  row_vector[Nta_total] Yobs; // Array of individual responses.
  row_vector[Nta_total] Tobs; // Individual timepoints.
  real Ythreshold; // Censoring threshold.

  // Matrix of individuals x observed tumor assessments (sparse matrix of 0s and 1s),
  // so the dimension is Nind x Nta_obs_y.
  int<lower=1> n_w_mat_inds_obs_y;
  vector[n_w_mat_inds_obs_y] w_mat_inds_obs_y;
  int<lower=1> n_v_mat_inds_obs_y;
  int v_mat_inds_obs_y[n_v_mat_inds_obs_y];
  int<lower=1> n_u_mat_inds_obs_y;
  int u_mat_inds_obs_y[n_u_mat_inds_obs_y];

  // Matrix of individuals x censored tumor assessments (sparse matrix of 0s and 1s).
  // so the dimension is Nind x Nta_cens_y.
  int<lower=1> n_w_mat_inds_cens_y;
  vector[n_w_mat_inds_cens_y] w_mat_inds_cens_y;
  int<lower=1> n_v_mat_inds_cens_y;
  int v_mat_inds_cens_y[n_v_mat_inds_cens_y];
  int<lower=1> n_u_mat_inds_cens_y;
  int u_mat_inds_cens_y[n_u_mat_inds_cens_y];
