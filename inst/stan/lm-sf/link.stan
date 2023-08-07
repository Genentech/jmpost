
functions {
    //
    // Source - lm-sf/link.stan
    //
    matrix link_contribution(matrix time, matrix pars_lm) {
        
        int nrows = rows(time);
        int ncols = cols(time);

        matrix[nrows, ncols] matrix_link_contrib = rep_matrix(0, nrows, ncols);
        vector[nrows] psi_bsld = col(pars_lm, 1);
        vector[nrows] psi_ks = col(pars_lm, 2);
        vector[nrows] psi_kg = col(pars_lm, 3);
        
        {% for item in items -%}
        real {{ item.parameter }} = col(pars_lm, {{loop.index1 + 3}})[1];
        matrix_link_contrib += {{ item.parameter }} .* {{ item.contribution_function }}(
            time,
            psi_bsld,
            psi_ks,
            psi_kg
        );
        {% endfor -%}
        return matrix_link_contrib;
    }
}


parameters{
    //
    // Source - lm-sf/link.stan
    //
    {% for item in items -%}
    real {{ item.parameter }};
    {% endfor -%}
}

transformed parameters {
    //
    // Source - lm-sf/link.stan
    //
    matrix[Nind, {{ 3 + length(items)}}] pars_lm = rep_matrix( 0, Nind, {{ 3 + length(items)}});
    
    pars_lm[,1] = lm_sf_psi_bsld;
    pars_lm[,2] = lm_sf_psi_ks;
    pars_lm[,3] = lm_sf_psi_kg;
    {% for item in items -%}
    pars_lm[,{{3 + loop.index1}}] = rep_vector(1, Nind) .* {{ item.parameter }};
    {% endfor -%}
}

