
functions {
    //
    // Source - base/link.stan
    //
    matrix link_contribution(
        matrix time,
        matrix link_function_inputs,
        vector link_coefficients
    ) {
        
        int nrows = rows(time);
        int ncols = cols(time);

        matrix[nrows, ncols] matrix_link_contrib = rep_matrix(0, nrows, ncols);
        
        {% for item in items -%}
        matrix_link_contrib += link_coefficients[{{loop.index1}}] .* {{ item }}_contrib(
            time,
            link_function_inputs
        );
        {% endfor -%}
        return matrix_link_contrib;
    }
}

parameters{
    //
    // Source - base/link.stan
    //
    {% for item in items -%}
    real {{ item }};
    {% endfor -%}

}

transformed parameters {
    vector[{{ length(items) }}] link_coefficients;
    {% for item in items -%}
    link_coefficients[{{loop.index1}}] = {{ item }};
    {% endfor -%}
}

