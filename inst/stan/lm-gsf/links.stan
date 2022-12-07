
////
////
//// DELETE ME
////
////


//
// signature:  contrib(time, bsld, ks, kg, phi)
//
// items = {
//     {
//         "parameter": "gamma",
//         "contribution_function": "get_ttg_contrib",
//     },
//     {
//         "parameter": "beta",
//         "contribution_function": "get_dtsld_contrib",
//     }
// }
//

// Need to iterate over:
//    - Link contribution
//    - Parameters  (include entries for gamma/beta)
//    - Transformed parameters (include entries for gamma/beta)
//
// Need to include:
//    - User defined link functions


functions {
    matrix link_contribution(matrix time, matrix pars_lm) {
        
        matrix[rows(time), cols(time)] link_contribution;

        vector[rows(pars_lm)] real psi_bsld = pars_lm[1];
        vector[rows(pars_lm)] real psi_ks = pars_lm[2];
        vector[rows(pars_lm)] real psi_kg = pars_lm[3];
        vector[rows(pars_lm)] real psi_phi = pars_lm[4];
        
        {% for item in items -%}
            vector[rows(pars_lm)] real {{ item.parameter }} pars_lm[{{loop.index1 + 4}}]
            
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


// row_vector[cols(time)] ttg_contribution = ttg(psi_ks, psi_kg, psi_phi);
// matrix[rows(time), cols(time)] ttg_contribution_matrix = rep_matrix(ttg_contribution, rows(time));
// link_contribution += gamma .* ttg_contribution_matrix;


// matrix[rows(time), cols(time)] dt = dtsld(time, psi_bsld, psi_ks, psi_kg, psi_phi);
// link_contribution += beta .* dt;

parameters{
    {% if ttg -%}
        real gamma;
    {% endif %}
    
    {% if dt -%}
        real beta;
    {% endif %}
}

transformed parameters {
   matrix[Nind, 6] pars_lm = append_col(
        psi_bsld,
        psi_ks,
        psi_kg,
        psi_phi
        rep_matrix(1, Nind, 1)  {% if ttg -%} * gamma {% endif %},
        rep_matrix(1, Nind, 1)  {% if dt -%} * beta {% endif %},
    );
}

