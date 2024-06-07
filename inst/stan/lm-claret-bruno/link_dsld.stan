
functions {
    //
    // Source - lm-claret-bruno/link_dsld.stan
    //
    matrix link_dsld_contrib(
        matrix time,
        matrix link_function_inputs
    ) {
        int nrows = rows(link_function_inputs);
        int ncols = cols(time);
        vector[nrows] ind_b = link_function_inputs[,1];
        vector[nrows] ind_g = link_function_inputs[,2];
        vector[nrows] ind_c = link_function_inputs[,3];
        vector[nrows] ind_p = link_function_inputs[,4];
        
        // Here we assume that psi's are replicated along the rows of the time matrix.
        matrix[nrows, ncols] ind_b_matrix = rep_matrix(ind_b, ncols);
        matrix[nrows, ncols] ind_g_matrix = rep_matrix(ind_g, ncols);
        matrix[nrows, ncols] ind_c_matrix = rep_matrix(ind_c, ncols);
        matrix[nrows, ncols] ind_p_matrix = rep_matrix(ind_p, ncols);

        matrix[nrows, ncols] result_lt0 = fmin(
            8000.0,
            ind_b_matrix .* ind_g_matrix .* exp(ind_g_matrix .* time)
        );

        matrix[nrows, ncols] result_gt0 = fmin(
            8000.0,
            ind_b_matrix .* ( 
                ind_g_matrix - ind_p_matrix .* exp(-ind_c_matrix .* time) .*
                exp(
                    (ind_g_matrix .* time) -
                    (ind_p_matrix ./ ind_c_matrix) .*
                    (1 -  exp(- ind_c_matrix .* time))
                )
            )
        );



        return if_lt0_else(time, result_gt0, result_lt0);
    }
}
