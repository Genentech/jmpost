row_vector[Nind] psi_bsld = exp(log(mu_bsld[study_index]) + eta_tilde_bsld * omega_bsld);
  row_vector[Nind] psi_ks = exp(log(mu_ks[arm_index]) + eta_tilde_ks * omega_ks);
  row_vector[Nind] psi_kg = exp(log(mu_kg[arm_index]) + eta_tilde_kg * omega_kg);
  row_vector[Nind] psi_phi =  inv_logit(logit(mu_phi[arm_index]) + eta_tilde_phi * omega_phi);

  // Log-likelihood values for using the loo package.
  row_vector[Nind] log_lik;
  row_vector[Nta_total] Ypred;

  // Log-survival values as we need them for generated quantitities.
  row_vector[Nind] log_surv_vals;

  log_lik = rep_row_vector(0.0, Nind);

  Ypred = sld(Tobs,
  psi_bsld[ind_index], psi_ks[ind_index], psi_kg[ind_index], psi_phi[ind_index]);
  log_lik += csr_matrix_times_vector(Nind, Nta_obs_y, w_mat_inds_obs_y, v_mat_inds_obs_y, u_mat_inds_obs_y,
  vect_normal_log_dens(Yobs[obs_y_index], Ypred[obs_y_index], Ypred[obs_y_index] * sigma)')';
  log_lik += csr_matrix_times_vector(Nind, Nta_cens_y, w_mat_inds_cens_y, v_mat_inds_cens_y, u_mat_inds_cens_y,
  vect_normal_log_cum(Ythreshold, Ypred[cens_y_index], Ypred[cens_y_index] * sigma)')';
