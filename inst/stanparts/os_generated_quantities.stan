  matrix[n_arms, n_os_pred_times] mean_unconditional_survival;
  matrix[n_arms, n_os_pred_times] mean_conditional_survival;
  matrix[n_arms, n_os_pred_times] mean_hazard;
  matrix[n_arms, n_os_pred_times] mean_log_hazard;
  matrix[n_arms, n_os_pred_times] adjusted_pop_log_hazard;

  matrix[n_save_individual, n_os_pred_times] save_ind_unconditional_survival;
  matrix[n_save_individual, n_os_pred_times] save_ind_conditional_survival;
  matrix[n_save_individual, n_os_pred_times] save_ind_log_hazard;
  row_vector[n_save_individual] save_ind_ttg;
  matrix[n_save_individual, n_os_pred_times] save_ind_dtsld;

  // Local scope - these will not be returned, only used here temporarily.
  {
    matrix[Nind, n_os_pred_times] ind_unconditional_survival;
    matrix[Nind, n_os_pred_times] ind_conditional_survival;
    matrix[Nind, n_os_pred_times] ind_log_hazard;
    int i_rep[n_os_pred_times];
    real surv_prob_at_cens;
    int index_pos;

    // Process each patient.
    for (i in 1:Nind) {
      i_rep = rep_array(i, n_os_pred_times);

      // Unconditional survival.
      ind_unconditional_survival[i] = exp(log_survival(os_pred_times,
      lambda, p, <link_arguments_as_par>
      psi_bsld[i_rep], psi_ks[i_rep], psi_kg[i_rep], psi_phi[i_rep],
      nodes, weights, beta_os_cov, os_cov_design[i_rep]));

      // Conditional survival.
      if (Death[i]) {
        ind_conditional_survival[i] = get_step_survival(Times[i], os_pred_times);
      } else {
        surv_prob_at_cens = exp(log_surv_vals[i]);
        ind_conditional_survival[i] = get_cond_survival(surv_prob_at_cens,
        ind_unconditional_survival[i]);
      }

      // Log hazard.
      ind_log_hazard[i] = to_row_vector(log_hazard(to_matrix(os_pred_times),
      lambda, p,  <link_arguments_as_par>
      psi_bsld[i_rep], psi_ks[i_rep], psi_kg[i_rep], psi_phi[i_rep],
      beta_os_cov, os_cov_design[i_rep]));
    }

    // Process each treatment arm.
    index_pos = 1;
    for (j in 1:n_arms) {
      mean_unconditional_survival[j] =
      row_means(ind_unconditional_survival[segment(index_per_arm, index_pos, n_index_per_arm[j])]);

      mean_conditional_survival[j] =
      row_means(ind_conditional_survival[segment(index_per_arm, index_pos, n_index_per_arm[j])]);

      mean_log_hazard[j] =
      row_means(ind_log_hazard[segment(index_per_arm, index_pos, n_index_per_arm[j])]);

      mean_hazard[j] =
      row_means(exp(ind_log_hazard[segment(index_per_arm, index_pos, n_index_per_arm[j])]));

      adjusted_pop_log_hazard[j] = pop_log_hazard(os_pred_times,
      <link_arguments_as_par>
      mu_bsld[arm_to_study_index[j]], mu_ks[j], mu_kg[j], mu_phi[j]);

      index_pos = index_pos + n_index_per_arm[j];
    }

    // Save specific individual quantities.
    save_ind_unconditional_survival = ind_unconditional_survival[index_save_individual];
    save_ind_conditional_survival = ind_conditional_survival[index_save_individual];
    save_ind_log_hazard = ind_log_hazard[index_save_individual];
    save_ind_ttg = ttg(psi_ks[index_save_individual], psi_kg[index_save_individual], psi_phi[index_save_individual]);
    save_ind_dtsld = (dtsld(rep_matrix(os_pred_times', n_save_individual),
    psi_bsld[index_save_individual], psi_ks[index_save_individual], psi_kg[index_save_individual], psi_phi[index_save_individual]))';

  }
