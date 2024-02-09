functions {

        ,
        

{{ stan.functions }}


    //
    // Source - base/survival.stan
    //

    // Log Hazard function
    matrix log_hazard(
        matrix time,
        vector pars_os,
        matrix link_function_inputs,
        vector link_coefficients
        vector cov_contribution
    ) {
        //print([rows(time), cols(time), rows(cov_contribution), cols(cov_contribution)]);
        matrix[rows(time), cols(time)] log_baseline = log_h0(time, pars_os);
        matrix[rows(time), cols(time)] cov_contribution_matrix = rep_matrix(
            cov_contribution,
            cols(time)
        );

        matrix[rows(time), cols(time)] lm_link_contribution = link_contribution(
            time,
            link_function_inputs,
            link_coefficients
        );

        matrix[rows(time), cols(time)] result =
            cov_contribution_matrix +
            lm_link_contribution +
            log_baseline;
        return result;
    }


    // Survival function
    // now we take care of time = 0 values - for these we just directly know that the result is 0.
    vector log_survival(
        vector time,
        vector pars_os,
        matrix link_function_inputs,
        vector link_coefficients
        data vector nodes,
        data vector weights,
        vector cov_contribution
    ) {
        // Caps the log hazard at 10000
        matrix[rows(time), rows(nodes)] node_times =  (time / 2) * ((nodes + 1)') ;
        matrix[rows(time), rows(nodes)] nodes_time_hazard = fmin(
            10000,
            exp(
                log_hazard(
                    node_times,
                    pars_os,
                    link_function_inputs,
                    link_coefficients,
                    cov_contribution
                )
            )
        );
        vector[rows(time)] result = - (time / 2) .* (nodes_time_hazard * weights);
        return result;
    }
    
    
    // Expand design matrix by parameter vector
    vector get_os_cov_contribution(matrix design, vector beta) {
        int n = rows(design);
        int p = cols(design);
        vector[n] cov_contribution;
        if (p == 0) {
            // 0 Vector to cover case where no covariates are defined
            // (e.g. intercept only model)
            cov_contribution = rep_vector(0, n);
            return cov_contribution;
        }
        if (n == 1) {
            cov_contribution[1] = (design * beta)[1];
            return cov_contribution;
        }
        cov_contribution = (design * beta);
        return cov_contribution;
    }


    matrix generate_log_hazard_estimates(
        array[] int pt_select_index,
        vector sm_time_grid,
        vector pars_os,
        matrix link_function_inputs,
        vector link_coefficients,
        vector cov_contribution
    ) {
        int n_pt_select_index = num_elements(pt_select_index);
        int n_sm_time_grid = num_elements(sm_time_grid);
        matrix[n_pt_select_index, n_sm_time_grid] result;
        for (i in 1:n_pt_select_index) {
            int current_pt_index = pt_select_index[i];
            result[i, ] = to_row_vector(
                log_hazard(
                    rep_matrix(sm_time_grid, 1),
                    pars_os,
                    rep_matrix(link_function_inputs[current_pt_index, ], n_sm_time_grid),
                    link_coefficients,
                    rep_vector(cov_contribution[current_pt_index], n_sm_time_grid)
                )
            );
        }
        return result;
    }


    matrix generate_log_survival_estimates(
        array[] int pt_select_index,
        vector sm_time_grid,
        vector pars_os,
        matrix link_function_inputs,
        vector link_coefficients,
        data vector nodes,
        data vector weights,
        vector cov_contribution
    ) {
        int n_pt_select_index = num_elements(pt_select_index);
        int n_sm_time_grid = num_elements(sm_time_grid);
        matrix[n_pt_select_index, n_sm_time_grid] result;
        for (i in 1:n_pt_select_index) {
            int current_pt_index = pt_select_index[i];
            result[i, ] = to_row_vector(
                log_survival(
                    sm_time_grid,
                    pars_os,
                    rep_matrix(link_function_inputs[current_pt_index, ], n_sm_time_grid),
                    link_coefficients,
                    nodes,
                    weights,
                    rep_vector(cov_contribution[current_pt_index], n_sm_time_grid)
                )
            );
        }
        return result;
    }
}



data{
    //
    // Source - base/survival.stan
    //

    int<lower=1> Nind_dead;            // Number of dead individuals (observed survival time).
    array[Nind_dead] int dead_ind_index;     // Index of dead individuals (observed survival time).
    vector[Nind] Times;
    int<lower=0> p_os_cov_design;
    matrix[Nind, p_os_cov_design] os_cov_design;

    // Integration parameters ----
    // These are the x positions and weights required to evaluate a polynomial function
    // between 0 and 1
    int<lower=1> n_nodes;
    vector[n_nodes] nodes;
    vector<lower=0, upper=1>[n_nodes] weights;
}


transformed data {
    array[rows(Times)] int time_positive = is_positive(Times);
    int n_positive = sum(time_positive);
    array[n_positive] int time_positive_index = which(time_positive);

{{ stan.transformed_data }}
}



parameters {
    //
    // Source - base/survival.stan
    //

    // Covariate coefficients.
    
    vector[p_os_cov_design] beta_os_cov;
    
{{ stan.parameters }}
}





transformed parameters {
    //
    // Source - base/survival.stan
    //

    // Calculate covariate contributions to log hazard function
    vector[Nind] os_cov_contribution = get_os_cov_contribution(
        os_cov_design,
        beta_os_cov
    );

{{ stan.transformed_parameters }}

    //
    // Source - base/survival.stan
    //

    // Log of survival function at the observed time points.
    vector[Nind] log_surv_fit_at_obs_times;
    log_surv_fit_at_obs_times = rep_vector(0.0, Nind);
    log_surv_fit_at_obs_times[time_positive_index] += log_survival(
        Times[time_positive_index],
        pars_os,
        link_function_inputs[time_positive_index],
        link_coefficients,
        nodes,
        weights,
        os_cov_contribution[time_positive_index]
    );

    // We always add the log-survival to the log-likelihood.
    log_lik += log_surv_fit_at_obs_times;

    // In case of death we add the log-hazard on top.
    log_lik[dead_ind_index] += to_vector(
        log_hazard(
            to_matrix(Times[dead_ind_index]),
            pars_os,
            link_function_inputs[dead_ind_index],
            link_coefficients,
            os_cov_contribution[dead_ind_index]
        )
    );
}


model{
    {{ stan.model }}
}


generated quantities {
    //
    // Source - base/survival.stan
    //
    matrix[n_pt_select_index, n_sm_time_grid] log_surv_fit_at_time_grid;
    matrix[n_pt_select_index, n_sm_time_grid] log_haz_fit_at_time_grid;
    if (n_sm_time_grid > 0) {
        log_surv_fit_at_time_grid = generate_log_survival_estimates(
            pt_select_index,
            sm_time_grid,
            pars_os,
            link_function_inputs,
            link_coefficients,
            nodes,
            weights,
            os_cov_contribution
        );
        log_haz_fit_at_time_grid = generate_log_hazard_estimates(
            pt_select_index,
            sm_time_grid,
            pars_os,
            link_function_inputs,
            link_coefficients,
            os_cov_contribution
        );
    }
}
