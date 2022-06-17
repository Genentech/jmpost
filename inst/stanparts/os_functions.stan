  matrix log_h0(matrix time, real lambda, real p) {
    matrix[rows(time), cols(time)] lambda_t = lambda * time;
    matrix[rows(time), cols(time)] log_num = log(lambda) + log(p) + (p - 1) * log(lambda_t);
    matrix[rows(time), cols(time)] log_denom = log1p(lambda_t ^ p);
    matrix[rows(time), cols(time)] result = log_num - log_denom;
    return result;
  }

  matrix log_hazard(matrix time,
  real lambda, real p, <link_arguments>
  row_vector psi_bsld, row_vector psi_ks, row_vector psi_kg, row_vector psi_phi,
  vector beta_os_cov, data matrix os_cov_design) {
    row_vector[rows(os_cov_design)] cov_contribution;
    matrix[rows(time), cols(time)] log_baseline = log_h0(time, lambda, p);
    if (rows(os_cov_design) > 1) {
      cov_contribution = (os_cov_design * beta_os_cov)';
    } else {
      cov_contribution[1] = (os_cov_design * beta_os_cov)[1];
    }
    matrix[rows(time), cols(cov_contribution)] cov_contribution_matrix = rep_matrix(cov_contribution, rows(time));
    <link_calculations>;
    matrix[rows(time), cols(time)] result =  <link_log_hazard_contribution> + log_baseline + cov_contribution_matrix;
    return result;
  }

  row_vector log_survival(row_vector time, real lambda,
  real p, <link_arguments>
  row_vector psi_bsld, row_vector psi_ks, row_vector psi_kg, row_vector psi_phi,
  data vector nodes, data row_vector weights, vector beta_os_cov, data matrix os_cov_design) {
    int time_positive[cols(time)] = is_positive(time);
    int n_positive = sum(time_positive);
    int time_positive_index[n_positive] = which(time_positive);
    row_vector[cols(time)] result = rep_row_vector(0.0, cols(time));

    matrix[rows(nodes), n_positive] nodes_time = (nodes + 1) * (time[time_positive_index] / 2);
    matrix[rows(nodes), n_positive] nodes_time_hazard = fmin(8000.0, exp(log_hazard(nodes_time, lambda,
    p, <link_arguments_as_par>
    psi_bsld[time_positive_index],
    psi_ks[time_positive_index],
    psi_kg[time_positive_index],
    psi_phi[time_positive_index], beta_os_cov, os_cov_design[time_positive_index])));
    result[time_positive_index] = - (weights * nodes_time_hazard) .* time[time_positive_index] / 2;
    return result;
  }

  real neg_log_sqrt_2_pi() {
    return -0.9189385332046727;
  }


  row_vector vect_normal_log_dens(row_vector y, row_vector mu, row_vector sigma) {
    row_vector[num_elements(y)] y_stand = (y - mu) ./ sigma;
    row_vector[num_elements(y)] main_result = - (y_stand .* y_stand) / 2;
    return main_result + neg_log_sqrt_2_pi() - log(sigma);
  }

  row_vector vect_normal_log_cum(real quantile, row_vector mu, row_vector sigma) {
    row_vector[num_elements(mu)] quant_stand = (quantile - mu) ./ sigma;
    row_vector[num_elements(mu)] cdf_vals = Phi(quant_stand);
    return log(cdf_vals);
  }

  row_vector row_means(matrix x) {
    row_vector[cols(x)] result = rep_row_vector(1.0 / rows(x), rows(x)) * x;
    return result;
  }



  row_vector pop_log_hazard(row_vector time,
  <link_arguments>
  real mu_bsld, real mu_ks, real mu_kg, real mu_phi) {

    row_vector[cols(time)] pop_dtsld = dtsld_for_hr(time, mu_bsld, mu_ks, mu_kg, mu_phi);
    real pop_ttg = ttg_for_hr(mu_ks, mu_kg, mu_phi);
    row_vector[cols(time)] result = 0 <Link_pop_log_haz>;
    return result;
  }

  row_vector get_step_survival(real death_time, row_vector times) {
    row_vector[cols(times)] result;
    for (i in 1:cols(times)) {
      result[i] = times[i] < death_time;
    }
    return result;
  }


  row_vector get_cond_survival(real cens_surv_prob, row_vector uncond_surv_probs) {
    row_vector[cols(uncond_surv_probs)] result = fmin(1.0, uncond_surv_probs / cens_surv_prob);
    return result;
