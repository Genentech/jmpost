
functions {
    // Define required function for enabling generated quantities
    vector lm_predict_value(vector time, matrix long_gq_parameters) {
        return sld(
            time,
            long_gq_parameters[,1],  // baseline
            long_gq_parameters[,2],  // shrinkage
            long_gq_parameters[,3]   // growth
        );
    }
}


generated quantities {
    // Enable individual subject predictions / quantities e.g.
    // `GridFixed()` / `GridObservation()` / `GridGrouped()` / `GridEven
    matrix[n_subjects, 3] long_gq_parameters;
    long_gq_parameters[, 1] = baseline_idv;
    long_gq_parameters[, 2] = shrinkage_idv;
    long_gq_parameters[, 3] = growth_idv;

    // Enable Population level predictions / quantities by taking the median of the
    // hierarchical distribution e.g. `GridPopulation()`
    matrix[gq_n_quant, 3] long_gq_pop_parameters;
    long_gq_pop_parameters[, 1] = rep_vector(mu_baseline, gq_n_quant);
    long_gq_pop_parameters[, 2] = rep_vector(mu_shrinkage, gq_n_quant);
    long_gq_pop_parameters[, 3] = rep_vector(mu_growth, gq_n_quant);
}
