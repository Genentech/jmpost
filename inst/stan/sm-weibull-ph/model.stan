

functions {
    // SurvivalWeibullPH
    matrix log_h0(matrix time, vector pars_os) {
        real lambda = pars_os[1];
        real gamma = pars_os[2];
        matrix[rows(time), cols(time)] result;
        result = log(lambda) + log(gamma) + (gamma - 1) * log(time);
        return result;
    }
}


parameters {
    // SurvivalWeibullPH
    real<lower=0.000000000001> sm_weibull_ph_lambda;
    real<lower=0.0000001> sm_weibull_ph_gamma;
}


transformed parameters {
    // SurvivalWeibullPH
    vector[2] pars_os = [sm_weibull_ph_lambda, sm_weibull_ph_gamma]';
}

