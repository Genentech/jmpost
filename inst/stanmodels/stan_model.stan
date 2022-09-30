functions {
    bla  matrix log_h0(matrix time, real lambda, real p) {
    matrix[rows(time), cols(time)] lambda_t = lambda * time;
    matrix[rows(time), cols(time)] log_num = log(lambda) + log(p) + (p - 1) * log(lambda_t);
    matrix[rows(time), cols(time)] log_denom = log1p(lambda_t ^ p);
    matrix[rows(time), cols(time)] result = log_num - log_denom;
    return result;
    }
    matrix log_hazard(matrix time,
    real lambda, real p, real beta_ttg,
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
    matrix[rows(time), cols(time)] result =  beta_ttg* rep_matrix(ttg(psi_ks, psi_kg, psi_phi), rows(time)) + log_baseline + cov_contribution_matrix;
    return result;
    }
    row_vector log_survival(row_vector time, real lambda,
    real p, real beta_ttg,
    row_vector psi_bsld, row_vector psi_ks, row_vector psi_kg, row_vector psi_phi,
    data vector nodes, data row_vector weights, vector beta_os_cov, data matrix os_cov_design) {
    int time_positive[cols(time)] = is_positive(time);
    int n_positive = sum(time_positive);
    int time_positive_index[n_positive] = which(time_positive);
    row_vector[cols(time)] result = rep_row_vector(0.0, cols(time));
    matrix[rows(nodes), n_positive] nodes_time = (nodes + 1) * (time[time_positive_index] / 2);
    matrix[rows(nodes), n_positive] nodes_time_hazard = fmin(8000.0, exp(log_hazard(nodes_time, lambda,
    p, beta_ttg,
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
parameters {
    real<lower=0> p;
    real<lower=0> lambda; // For the log-logistic baseline hazard.
    real beta_ttg;
    vector[p_os_cov_design] beta_os_cov; // Covariate coefficients.
}
transformed parameters {
    log_surv_vals = log_survival(Times, lambda, p, beta_ttg,
    psi_bsld, psi_ks, psi_kg, psi_phi,
    nodes, weights, beta_os_cov, os_cov_design);
    log_lik += log_surv_vals;
    log_lik[dead_ind_index] += to_row_vector(log_hazard(to_matrix(Times[dead_ind_index]), lambda, p, beta_ttg,
    psi_bsld[dead_ind_index], psi_ks[dead_ind_index], psi_kg[dead_ind_index], psi_phi[dead_ind_index],
    beta_os_cov, os_cov_design[dead_ind_index]));
}
model {
    p ~ gamma(2, 0.5);
    1/lambda ~ lognormal(0, 5);
    beta_os_cov ~ normal(0, 5);
    target+=sum(log_lik);
    
}
generated quantities {
    matrix[n_arms, n_os_pred_times] mean_unconditional_survival;
    matrix[n_arms, n_os_pred_times] mean_conditional_survival;
    matrix[n_arms, n_os_pred_times] mean_hazard;
    matrix[n_arms, n_os_pred_times] mean_log_hazard;
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
    lambda, p, beta_ttg,
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
    lambda, p,  beta_ttg,
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

