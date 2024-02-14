
functions {
    //
    // Source - lm-gsf/link_identity.stan
    //

    // Identity of SLD
    matrix link_identity_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int ncols = cols(time);
        int nrows = rows(time);
        matrix[nrows, ncols] intercept = rep_matrix(link_function_inputs[,1], ncols);
        matrix[nrows, ncols] slope = rep_matrix(link_function_inputs[,2], ncols);
        return intercept + (slope .* time);
    }
}


