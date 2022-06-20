functions {
row_vector ifelse(int[] condition, row_vector yes, row_vector no) {
    row_vector[num_elements(yes)] result;
    for (i in 1:num_elements(yes)) {
      result[i] = condition[i] ? yes[i] : no[i];
    }
    return result;
  }

  int[] is_negative(row_vector x) {
    int result[num_elements(x)];
    for (i in 1:num_elements(x)) {
      result[i] = x[i] < 0.0;
    }
    return result;
  }

  int[] is_positive(row_vector x) {
    return is_negative(- x);
  }

  int[] which(int[] x) {
    int len = sum(x);
    int ret[len];
    int pointer = 0;
    for (i in 1:num_elements(x)) {
      if (x[i] != 0) {
        if (x[i] != 1) {
          reject("integer array passed to `which` function must only contain 0 or 1");
        }
        pointer = pointer + 1;
        ret[pointer] = i;
      }
    }
    return ret;
  }

  //- sld ----
  // SLD model (GSF)

  row_vector sld(row_vector time, row_vector psi_bsld, row_vector psi_ks,
  row_vector psi_kg, row_vector psi_phi) {
    row_vector[num_elements(time)] psi_phi_mod = ifelse(is_negative(time), zeros_row_vector(num_elements(time)), psi_phi);
    row_vector[num_elements(time)] result = fmin(
      8000.0,
      psi_bsld .* (psi_phi_mod .* exp(- psi_ks .* time) + (1 - psi_phi_mod) .* exp(psi_kg .* time))
    );
    return result;
  }

  //- ttg ----
  // Time-to-growth (TTG)

  row_vector ttg(row_vector psi_ks, row_vector psi_kg, row_vector psi_phi) {
    row_vector[num_elements(psi_ks)] num = logit(psi_phi) + log(psi_ks ./ psi_kg);
    row_vector[num_elements(psi_ks)] denom = psi_ks + psi_kg;
    row_vector[num_elements(psi_ks)] result = num ./ denom;
    return result;
  }

  //- ttg_for_hr ----
  // TTG for HR calculation

  real ttg_for_hr(real mu_ks, real mu_kg, real mu_phi) {
    real num = logit(mu_phi) + log(mu_ks / mu_kg);
    real denom = mu_ks + mu_kg;
    real result = num / denom;
    return result;
  }

  //- dtsld ----
  // Derivative of SLD

  matrix dtsld(matrix time, row_vector psi_bsld, row_vector psi_ks,
  row_vector psi_kg, row_vector psi_phi) {
    // Here we assume that psi's are replicated along the rows of the time matrix.
    matrix[rows(time), cols(psi_bsld)] psi_bsld_matrix = rep_matrix(psi_bsld, rows(time));
    matrix[rows(time), cols(psi_ks)] psi_ks_matrix = rep_matrix(psi_ks, rows(time));
    matrix[rows(time), cols(psi_kg)] psi_kg_matrix = rep_matrix(psi_kg, rows(time));
    // We also assume that all the time values are positive. Therefore no need to change phi.
    matrix[rows(time), cols(psi_phi)] psi_phi_matrix = rep_matrix(psi_phi, rows(time));
    matrix[rows(time), cols(time)] result = fmin(
      8000.0,
      psi_bsld_matrix .* (
        (1 - psi_phi_matrix) .* psi_kg_matrix .* exp(psi_kg_matrix .* time) -
        psi_phi_matrix .* psi_ks_matrix .* exp(- psi_ks_matrix .* time)
      )
    );
    return result;
  }

  //- dtsld_for_hr ----
  // Derivative of SLD for HR calculation

  row_vector dtsld_for_hr(row_vector time, real mu_bsld, real mu_ks,
  real mu_kg, real mu_phi) {
    row_vector[cols(time)] result = fmin(
      8000.0,
      mu_bsld * ((1 - mu_phi) * mu_kg * exp(mu_kg * time) - mu_phi * mu_ks * exp(- mu_ks * time))
    );
    return result;
  }
  matrix log_h0(matrix time, real lambda, real p) {
    matrix[rows(time), cols(time)] lambda_t = lambda * time;
    matrix[rows(time), cols(time)] log_num = log(lambda) + log(p) + (p - 1) * log(lambda_t);
    matrix[rows(time), cols(time)] log_denom = log1p(lambda_t ^ p);
    matrix[rows(time), cols(time)] result = log_num - log_denom;
    return result;
  }

  matrix log_hazard(matrix time,
  real lambda, real p,  real beta_dt, real beta_ttg,
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
     matrix[rows(time), cols(time)] dt = dtsld(time, psi_bsld, psi_ks, psi_kg, psi_phi); row_vector[cols(time)] ttg_contribution = ttg(psi_ks, psi_kg, psi_phi);

    matrix[rows(time), cols(time)] ttg_contribution_matrix = rep_matrix(ttg_contribution, rows(time));;
    matrix[rows(time), cols(time)] result =   beta_dt * dt+ beta_ttg * ttg_contribution_matrix + log_baseline + cov_contribution_matrix;
    return result;
  }

  row_vector log_survival(row_vector time, real lambda,
  real p,  real beta_dt, real beta_ttg,
  row_vector psi_bsld, row_vector psi_ks, row_vector psi_kg, row_vector psi_phi,
  data vector nodes, data row_vector weights, vector beta_os_cov, data matrix os_cov_design) {
    int time_positive[cols(time)] = is_positive(time);
    int n_positive = sum(time_positive);
    int time_positive_index[n_positive] = which(time_positive);
    row_vector[cols(time)] result = rep_row_vector(0.0, cols(time));

    matrix[rows(nodes), n_positive] nodes_time = (nodes + 1) * (time[time_positive_index] / 2);
    matrix[rows(nodes), n_positive] nodes_time_hazard = fmin(8000.0, exp(log_hazard(nodes_time, lambda,
    p,   beta_dt,  beta_ttg,
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
   real beta_dt, real beta_ttg,
  real mu_bsld, real mu_ks, real mu_kg, real mu_phi) {

    row_vector[cols(time)] pop_dtsld = dtsld_for_hr(time, mu_bsld, mu_ks, mu_kg, mu_phi);
    real pop_ttg = ttg_for_hr(mu_ks, mu_kg, mu_phi);
    row_vector[cols(time)] result = 0  + beta_dt * pop_dtsld + beta_ttg * pop_ttg;
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
  }

}

data {
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
  row_vector[Nind] Times;
  int Death[Nind];
  int<lower=1> p_os_cov_design;
  matrix[Nind, p_os_cov_design] os_cov_design;


  int<lower=1> n_os_pred_times;
  row_vector<lower=0>[n_os_pred_times] os_pred_times;

  int<lower=1> n_nodes;
  vector[n_nodes] nodes;
  row_vector<lower=0, upper=1>[n_nodes] weights;

}

parameters{
real<lower=0, upper=100> mean_mu_ks;
  real<lower=0, upper=100> mean_mu_kg;
  real<lower=0.001, upper=0.999> mean_mu_phi;
  real<lower=0, upper=10> sd_mu_ks;
  real<lower=0, upper=10> sd_mu_kg;
  real<lower=0, upper=10> sd_mu_phi;

  // Population parameters.
  row_vector<lower=0>[n_studies] mu_bsld;
  row_vector<lower=0>[n_arms] mu_ks;
  row_vector<lower=0>[n_arms] mu_kg;
  row_vector<lower=0, upper=1>[n_arms] mu_phi;

  real<lower=0> omega_bsld;
  real<lower=0> omega_ks;
  real<lower=0> omega_kg;
  real<lower=0> omega_phi;

  // Standard deviation for RUV.
  real<lower=0.00001, upper=100> sigma;

  // Random effects.
  row_vector[Nind] eta_tilde_bsld;
  row_vector[Nind] eta_tilde_ks;
  row_vector[Nind] eta_tilde_kg;
  row_vector[Nind] eta_tilde_phi;
  real<lower=0> p;
  real<lower=0> lambda; // For the log-logistic baseline hazard.
  real beta_dt;
 real beta_ttg;

  vector[p_os_cov_design] beta_os_cov; // Covariate coefficients.

}

transformed parameters {
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
 log_surv_vals = log_survival(Times, lambda, p,   beta_dt,  beta_ttg,
  psi_bsld, psi_ks, psi_kg, psi_phi,
  nodes, weights, beta_os_cov, os_cov_design);
  log_lik += log_surv_vals;

  log_lik[dead_ind_index] += to_row_vector(log_hazard(to_matrix(Times[dead_ind_index]), lambda, p,   beta_dt,  beta_ttg,
  psi_bsld[dead_ind_index], psi_ks[dead_ind_index], psi_kg[dead_ind_index], psi_phi[dead_ind_index],
  beta_os_cov, os_cov_design[dead_ind_index]));

}

model {
mean_mu_ks~lognormal(1,0.5);
mean_mu_kg~lognormal(-0.36,1);
mean_mu_phi~beta(5,5);
sd_mu_ks~lognormal(0,0.5);
sd_mu_kg~lognormal(0,0.5);
sd_mu_phi~lognormal(0,0.5);
mu_bsld~lognormal(55,5);
omega_bsld~lognormal(0,1);
omega_ks~lognormal(0,1);
omega_kg~lognormal(0,1);
omega_phi~lognormal(0,1);
sigma~lognormal(-1.7,0.8);
eta_tilde_bsld~normal(0,5);
eta_tilde_ks~normal(0,5);
eta_tilde_kg~normal(0,5);
eta_tilde_phi~normal(0,5);
mu_ks[sld_par_shared]~lognormal(mean_mu_ks, sd_mu_ks);
mu_kg[sld_par_shared]~lognormal(mean_mu_kg, sd_mu_kg);
logit(mu_phi[sld_par_shared])~normal(logit(mean_mu_phi), sd_mu_phi);
mu_ks[sld_par_separate]~lognormal(1,0.5);
mu_kg[sld_par_separate]~lognormal(-0.36,1);
mu_phi[sld_par_separate]~beta(5,5);
p~gamma(2, 0.5);
1/lambda~lognormal(0, 5);
beta_os_cov~normal(0, 5);
beta_dt~normal(0,5);
beta_ttg~normal(0,5) ; target+=sum(log_lik);

}

generated quantities{
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
      lambda, p,   beta_dt,  beta_ttg,
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
      lambda, p,    beta_dt,  beta_ttg,
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
        beta_dt,  beta_ttg,
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

}


