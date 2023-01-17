
functions {
    // LinkNone - link_contribution just returns 0 no matter the input
    matrix link_contribution(matrix time, matrix pars_lm) {
        return  rep_matrix(0, rows(time), cols(time));
    }
}


transformed data {
    // LinkNone - Empty matrix to appease the type checking
    matrix[Nind, 0] pars_lm = rep_matrix(0, Nind, 0);
}
