
functions {
    //
    // Source - lm-gsf/link.stan
    //
    matrix link_contribution(matrix time, matrix pars_lm) {
        
        matrix[rows(time), cols(time)] link_contribution;

        vector[rows(pars_lm)] real psi_bsld = pars_lm[1];
        vector[rows(pars_lm)] real psi_ks = pars_lm[2];
        vector[rows(pars_lm)] real psi_kg = pars_lm[3];
        vector[rows(pars_lm)] real psi_phi = pars_lm[4];
        
        {% for item in items -%}
        real {{ item.parameter }} pars_lm[{{loop.index1 + 4}}][1];
        link_contribution += {{ item.parameter }} .* {{ item.contribution_function }}(
            time,
            psi_bsld,
            psi_ks,
            psi_kg,
            psi_phi
        );
        
        {% endfor -%}
        return link_contribution;
    }
}


parameters{
    //
    // Source - lm-gsf/link.stan
    //
    {% for item in items -%}
    real {{ item.parameter }};
    {% endfor -%}
}

transformed parameters {
    //
    // Source - lm-gsf/link.stan
    //
    matrix[Nind, {{ 4 + length(items)}}] pars_lm = append_col(
        lm_gsf_psi_bsld,
        lm_gsf_psi_ks,
        lm_gsf_psi_kg,
        lm_gsf_psi_phi,
        {% for item in items -%}
        rep_vector(1, Nind) .* {{ item.parameter }}  {% if not loop.is_last%},{%endif%}
        {% endfor -%}
    );
}

