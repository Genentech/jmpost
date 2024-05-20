
transformed parameters {
    // Define matrix required for link functions
    matrix[n_subjects, 3] link_function_inputs;
    link_function_inputs[,1] = baseline_idv;
    link_function_inputs[,2] = shrinkage_idv;
    link_function_inputs[,3] = growth_idv;
}
