

functions {
    // SurvivalExponential
    matrix log_h0(matrix time, row_vector pars_os) {
        real lambda = pars_os[1];
        matrix[rows(time), cols(time)] result = rep_matrix(
            log(lambda),
            rows(time),
            cols(time)
        );
        return result;
    }
}

parameters {
    // SurvivalExponential
    real<lower=0.00000000000001> sm_exponential_ph_lambda;
}


transformed parameters {
    // SurvivalExponential
    row_vector[1] pars_os = [sm_exponential_ph_lambda];
}

// TODO - Remove and make user inputs
model {
    // SurvivalExponential
    sm_exponential_ph_lambda ~ gamma(2, 0.5);
}
