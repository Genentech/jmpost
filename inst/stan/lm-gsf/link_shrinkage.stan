

functions {
    //
    // Source - lm-gsf/link_shrinkage.stan
    //
    matrix link_shrinkage_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        return rep_matrix(log(link_function_inputs[,2]), cols(time));
    }
}

