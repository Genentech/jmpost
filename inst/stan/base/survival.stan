functions {

{{ stan.functions }}

    // SurvivalModel
    // Log Hazard function
    matrix log_hazard(
        matrix time,
        row_vector pars_os,
        matrix pars_lm,
        vector cov_contribution
    ) {
        //print([rows(time), cols(time), rows(cov_contribution), cols(cov_contribution)]);
        matrix[rows(time), cols(time)] log_baseline = log_h0(time, pars_os);
        matrix[rows(time), cols(time)] cov_contribution_matrix = rep_matrix(
            cov_contribution,
            cols(time)
        );
        
        matrix[rows(time), cols(time)] lm_link_contribution = link_contribution(time, pars_lm);
        
        matrix[rows(time), cols(time)] result =
            cov_contribution_matrix +
            lm_link_contribution +
            log_baseline
            ;
        return result;
    }


    // SurvivalModel
    // Survival function
    // now we take care of time = 0 values - for these we just directly know that the result is 0.
    row_vector log_survival(
        row_vector time,
        row_vector pars_os,
        matrix pars_lm,
        data vector nodes,
        data row_vector weights,
        vector cov_contribution
    ) {
        // Caps the log hazard at 10000
        matrix[cols(time), rows(nodes)] node_times = ((nodes + 1) * (time / 2))';
        matrix[rows(nodes), cols(time)] nodes_time_hazard = fmin(
            10000,
            exp(
                log_hazard(
                    node_times,
                    pars_os,
                    pars_lm,
                    cov_contribution
                )
            )
        )';
        row_vector[cols(time)] result = - (weights * nodes_time_hazard) .* time / 2;
        return result;
    }

}



data{
    // SurvivalModel
    int<lower=1> Nind_dead;            // Number of dead individuals (observed survival time).
    array[Nind_dead] int dead_ind_index;     // Index of dead individuals (observed survival time).
    row_vector[Nind] Times;
    int<lower=1> p_os_cov_design;
    matrix[Nind, p_os_cov_design] os_cov_design;

    // SurvivalModel
    // Integration parameters ----
    // These are the x positions and weights required to evaluate a polynomial function
    // between 0 and 1
    int<lower=1> n_nodes;
    vector[n_nodes] nodes;
    row_vector<lower=0, upper=1>[n_nodes] weights;
}


transformed data {
    // SurvivalModel
    array[cols(Times)] int time_positive = is_positive(Times);
    int n_positive = sum(time_positive);
    array[n_positive] int time_positive_index = which(time_positive);
    
{{ stan.transformed_data }}
}



parameters {
    // SurvivalModel
    // Covariate coefficients.
    vector[p_os_cov_design] beta_os_cov;
{{ stan.parameters }}
}



transformed parameters {

    // SurvivalModel
    // Calculate coveriate contributions to log hazard function
    vector[rows(os_cov_design)] os_cov_contribution;
    if (rows(os_cov_design) > 1) {
        os_cov_contribution = (os_cov_design * beta_os_cov);
    } else {
        os_cov_contribution[1] = (os_cov_design * beta_os_cov)[1];
    }

{{ stan.transformed_parameters }}

    // SurvivalModel
    // Log-survival values as we need them for generated quantitities.
    // We always add the log-survival to the log-likelihood.
    log_lik[time_positive_index] += log_survival(
        Times[time_positive_index],
        pars_os,
        pars_lm[time_positive_index],
        nodes,
        weights,
        os_cov_contribution[time_positive_index]
    );

    // SurvivalModel
    // In case of death we add the log-hazard on top.
    log_lik[dead_ind_index] += to_row_vector(
        log_hazard(
            to_matrix(Times[dead_ind_index])',
            pars_os,
            pars_lm[dead_ind_index],
            os_cov_contribution[dead_ind_index]
        )
    );
}


model{
    // SurvivalModel
    beta_os_cov ~ normal(0, 5); // TODO - Move to r code?

    {{ stan.model }}
}

