

functions {
    // SurvivalLogLogistic
    matrix log_h0(matrix time, vector pars_os) {
        real lambda = pars_os[1];
        real p = pars_os[2];
        matrix[rows(time), cols(time)] result;
        result =log(lambda) + log(p) + (p-1)*(log(lambda)+log(time))- log(1 + (lambda .* time) .^p) ;
        return result;
    }
}


parameters {
    // SurvivalLogLogistic
    real<lower={{ machine_double_eps }}> sm_logl_lambda;
    real<lower={{ machine_double_eps }}> sm_logl_p;
}


transformed parameters {
    // SurvivalLogLogistic
    vector[2] pars_os = [sm_logl_lambda, sm_logl_p]';
}

