

functions {
    //
    // Source - lm-claret-bruno/link_growth.stan
    //
    matrix link_growth_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int ncols = cols(time);
        return rep_matrix(log(link_function_inputs[,2]), ncols);
    }
}
