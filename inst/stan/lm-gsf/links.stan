
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
    
    matrix ttg(
        matrix time,
        row_vector psi_bsld,
        row_vector psi_ks,
        row_vector psi_kg,
        row_vector psi_phi
    ) {
        row_vector[num_elements(psi_ks)] num = logit(psi_phi) + log(psi_ks ./ psi_kg);
        row_vector[num_elements(psi_ks)] denom = psi_ks + psi_kg;
        row_vector[num_elements(psi_ks)] ttg_contribution = num ./ denom;
        matrix[rows(time), cols(time)] ttg_contribution_matrix = rep_matrix(ttg_contribution, rows(time));
        return ttg_contribution_matrix;
    }

    // Derivative of SLD
    matrix dtsld(
        matrix time,
        row_vector psi_bsld,
        row_vector psi_ks,
        row_vector psi_kg,
        row_vector psi_phi
    ) {
        // Here we assume that psi's are replicated along the rows of the time matrix.
        matrix[rows(time), cols(psi_bsld)] psi_bsld_matrix = rep_matrix(psi_bsld, rows(time));
        matrix[rows(time), cols(psi_ks)] psi_ks_matrix = rep_matrix(psi_ks, rows(time));
        matrix[rows(time), cols(psi_kg)] psi_kg_matrix = rep_matrix(psi_kg, rows(time));
        // We also assume that all the time values are positive. Therefore no need to change phi.
        matrix[rows(time), cols(psi_phi)] psi_phi_matrix = rep_matrix(psi_phi, rows(time));
        matrix[rows(time), cols(time)] result = fmin(
            8000.0,
            psi_bsld_matrix .* (
                (1 - psi_phi_matrix) .* psi_kg_matrix .* exp(psi_kg_matrix .* time) -
                psi_phi_matrix .* psi_ks_matrix .* exp(- psi_ks_matrix .* time)
            )
        );
        return result;
    }
    

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

