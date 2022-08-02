 log_surv_vals = log_survival(Times, lambda, p, <link_log_surv>
  psi_bsld, psi_ks, psi_kg, psi_phi,
  nodes, weights, beta_os_cov, os_cov_design);
  log_lik += log_surv_vals;

  log_lik[dead_ind_index] += to_row_vector(log_hazard(to_matrix(Times[dead_ind_index]), lambda, p, <link_log_lik>
  psi_bsld[dead_ind_index], psi_ks[dead_ind_index], psi_kg[dead_ind_index], psi_phi[dead_ind_index],
  beta_os_cov, os_cov_design[dead_ind_index]));
