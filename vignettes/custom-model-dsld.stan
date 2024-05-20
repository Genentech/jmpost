
functions {
    // Provide definition for the dsld link function
    matrix link_dsld_contrib(matrix time, matrix link_function_inputs) {
        int nrow = rows(time);
        int ncol = cols(time);
        // broadcast input vectors to match the size of the time matrix
        matrix[nrow, ncol] baseline = rep_matrix(link_function_inputs[,1], ncol);
        matrix[nrow, ncol] shrinkage = rep_matrix(link_function_inputs[,2], ncol);
        matrix[nrow, ncol] growth = rep_matrix(link_function_inputs[,3], ncol);
        return growth - baseline .* shrinkage .* exp(- shrinkage .* time);
    }
}
