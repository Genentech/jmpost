

functions {
    // SurvivalExponential
    matrix log_h0(matrix time, vector pars_os) {
        real lambda = pars_os[1];
        matrix[rows(time), cols(time)] result = rep_matrix(
            log(lambda),
            rows(time),
            cols(time)
        );
        return result;
    }
}


transformed parameters {
    // SurvivalExponential
    real<lower={{ machine_double_eps }}> sm_exp_lambda = prior_const_sm_exp_lambda;
    vector[1] pars_os = [sm_exp_lambda]';
}

