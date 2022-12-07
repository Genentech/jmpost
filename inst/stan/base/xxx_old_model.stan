functions {

    //- neg_log_sqrt_2_pi ----
    // Constant used in below.
    // @class ALL
    real neg_log_sqrt_2_pi() {
        return -0.9189385332046727;
    }


    //- vect_normal_log_dens ----
    // Vectorized version of normal_lpdf, i.e. returns log normal density values.
    // @class ALL
    row_vector vect_normal_log_dens(row_vector y, row_vector mu, row_vector sigma) {
        row_vector[num_elements(y)] y_stand = (y - mu) ./ sigma;
        row_vector[num_elements(y)] main_result = - (y_stand .* y_stand) / 2;
        return main_result + neg_log_sqrt_2_pi() - log(sigma);
    }


    //- vect_normal_log_cum ----
    // Vectorized version of normal_lcdf, i.e. return log normal CDF values.
    // @class ALL
    row_vector vect_normal_log_cum(real quantile, row_vector mu, row_vector sigma) {
        row_vector[num_elements(mu)] quant_stand = (quantile - mu) ./ sigma;
        row_vector[num_elements(mu)] cdf_vals = Phi(quant_stand);
        return log(cdf_vals);
    }


    //- row_means ----
    // Means across the rows of a matrix
    // @class ALL
    row_vector row_means(matrix x) {
        row_vector[cols(x)] result = rep_row_vector(1.0 / rows(x), rows(x)) * x;
        return result;
    }


    //- ifelse ----
    // Vectorized ifelse() similar as in R.
    // @class ALL
    row_vector ifelse(array[] int condition, row_vector yes, row_vector no) {
        row_vector[num_elements(yes)] result;
        for (i in 1:num_elements(yes)) {
            result[i] = condition[i] ? yes[i] : no[i];
        }
        return result;
    }


    //- is_negative ----
    // Vectorized negative predicate. Basically returns ifelse(x < 0, 1, 0).
    array[] int is_negative(row_vector x) {
        array[num_elements(x)] int result;
        for (i in 1:num_elements(x)) {
            result[i] = x[i] < 0.0;
        }
        return result;
    }


    //- is_positive ----
    // Vectorized positive predicate. Basically returns ifelse(x > 0, 1, 0).
    array[] int is_positive(row_vector x) {
        return is_negative(- x);
    }


    //- which ----
    // which function. Returns which of the 0/1 elements are not 0.
    array[] int which(array[] int x) {
        int len = sum(x);
        array[len] int ret;
        int pointer = 0;
        for (i in 1:num_elements(x)) {
            if (x[i] != 0) {
                if (x[i] != 1) {
                    reject("integer array passed to `which` function must only contain 0 or 1");
                }
                pointer = pointer + 1;
                ret[pointer] = i;
            }
        }
        return ret;
    }

{{ functions }}


{% if include_hazard -%}
    //- log_hazard ----
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



    //- log_survival ----
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
{% endif %}

}



data{
    int<lower=1> Nind;                 // Number of individuals.
    
    
{% if include_hazard -%}
    // Survival data ----
    int<lower=1> Nind_dead;            // Number of dead individuals (observed survival time).
    array[Nind_dead] int dead_ind_index;     // Index of dead individuals (observed survival time).
    row_vector[Nind] Times;
    int<lower=1> p_os_cov_design;
    matrix[Nind, p_os_cov_design] os_cov_design;

    // Integration parameters ----
    // These are the x positions and weights required to evaluate a polynomial function
    // between 0 and 1
    int<lower=1> n_nodes;
    vector[n_nodes] nodes;
    row_vector<lower=0, upper=1>[n_nodes] weights;
{% endif %}
    
    
    // Longditudinal data
    int<lower=1> Nta_total;            // Total number of tumor assessments.
    // int<lower=1> Nta_obs_y;            // Number of observed tumor assessments (not censored).
    // int<lower=1> Nta_cens_y;           // Number of censored tumor assessments (below threshold).

    array[Nta_total] int ind_index;          // Index of individuals for each tumor assessment.
    // array[Nta_obs_y] int obs_y_index;        // Index of observed tumor assessments (not censored).
    // array[Nta_cens_y] int cens_y_index;      // Index of censored tumor assessments.

    // int<lower=1> n_studies;                                // Number of studies.
    // array[Nind] int<lower=1,upper=n_studies> study_index;  // Index of study for all individuals.
    // int<lower=1> n_arms;                                   // Number of treatment arms.
    // array[Nind] int<lower=1,upper=n_arms> arm_index;       // Index of treatment arm for all individuals.


    // TODO - Need to get a better understanding of this shared
    // int<lower=1> n_sld_par_shared;
    // array[n_sld_par_shared] int<lower=1,upper=n_arms> sld_par_shared;
    // int<lower=1> n_sld_par_separate;
    // array[n_sld_par_separate] int<lower=1,upper=n_arms> sld_par_separate;


    // array[n_arms] int<lower=1,upper=n_studies> arm_to_study_index;


    // Ragged index vector of individuals per treatment arm (see R code).
    // array[n_arms] int<lower=1,upper=Nind> n_index_per_arm;
    // array[Nind] int<lower=1,upper=Nind> index_per_arm;


    vector[Nta_total] Yobs;   // Array of individual responses.
    vector[Nta_total] Tobs;   // Individual timepoints.
    // real Ythreshold;              // Censoring threshold.


    // // Matrix of individuals x observed tumor assessments (sparse matrix of 0s and 1s),
    // // so the dimension is Nind x Nta_obs_y.
    // int<lower=1> n_w_mat_inds_obs_y;
    // vector[n_w_mat_inds_obs_y] w_mat_inds_obs_y;
    // int<lower=1> n_v_mat_inds_obs_y;
    // array[n_v_mat_inds_obs_y] int v_mat_inds_obs_y;
    // int<lower=1> n_u_mat_inds_obs_y;
    // array[n_u_mat_inds_obs_y] int u_mat_inds_obs_y;


    // // Matrix of individuals x censored tumor assessments (sparse matrix of 0s and 1s).
    // // so the dimension is Nind x Nta_cens_y.
    // int<lower=1> n_w_mat_inds_cens_y;
    // vector[n_w_mat_inds_cens_y] w_mat_inds_cens_y;
    // int<lower=1> n_v_mat_inds_cens_y;
    // array[n_v_mat_inds_cens_y] int v_mat_inds_cens_y;
    // int<lower=1> n_u_mat_inds_cens_y;
    // array[n_u_mat_inds_cens_y] int u_mat_inds_cens_y;

{{ data }}

}


transformed data {
{% if include_hazard -%}
    array[cols(Times)] int time_positive = is_positive(Times);
    int n_positive = sum(time_positive);
    array[n_positive] int time_positive_index = which(time_positive);
{% endif %}

{{ transformed_data }}
}



parameters{
{% if include_hazard -%}
    // Covariate coefficients.
    vector[p_os_cov_design] beta_os_cov;
{% endif %}
{{ parameters }}
}



transformed parameters{

    // Log-likelihood values for using the loo package.
    row_vector[Nind] log_lik = rep_row_vector(0.0, Nind);
    
{{ transformed_parameters }}

{% if include_hazard -%}
    // Calculate coveriate contributions to log hazard function
    vector[rows(os_cov_design)] os_cov_contribution;
    if (rows(os_cov_design) > 1) {
        os_cov_contribution = (os_cov_design * beta_os_cov);
    } else {
        os_cov_contribution[1] = (os_cov_design * beta_os_cov)[1];
    }
    

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


    // In case of death we add the log-hazard on top.
    log_lik[dead_ind_index] += to_row_vector(
        log_hazard(
            to_matrix(Times[dead_ind_index])',
            pars_os,
            pars_lm[dead_ind_index],
            os_cov_contribution[dead_ind_index]
        )
    );
{% endif %}

}


model{
{% if include_hazard -%}
    beta_os_cov ~ normal(0, 5); // TODO - Move to r code?
{% endif %}
{{ model }}
    target += sum(log_lik);
}

