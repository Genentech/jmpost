generated quantities {
    matrix[gq_n_quant, 3] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = exp(
        rep_vector(log_mu_baseline, gq_n_quant) +
        gq_baseline_cov_design * beta_baseline
    );
    long_gq_pop_parameters[, 2] = rep_vector(mu_shrinkage, gq_n_quant);
    long_gq_pop_parameters[, 3] = exp(
        rep_vector(log_mu_growth, gq_n_quant) +
        gq_growth_cov_design * beta_growth
    );
}
