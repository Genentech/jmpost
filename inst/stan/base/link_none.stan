

functions {
    //
    // Source - base/link_none.stan
    //
    // If user has requested link_none then provide a dummy link_contribution function
    // that does nothing
    matrix link_contribution(matrix time, matrix pars_lm) {
        return  rep_matrix(0, rows(time), cols(time));
    }
}

transformed data {
    //
    // Source - base/link_none.stan
    //
    // If user has requested link_none then provide a dummy pars_lm object
    // that contains nothing
    matrix[Nind, 0] link_function_inputs = rep_matrix(0, Nind, 0);
    vector link_coefficients[0];
}


