

functions {
    // SurvivalLogLogistic
    matrix log_h0(matrix time, vector pars_os) {
        real a = pars_os[1];
        real b = pars_os[2];
        matrix[rows(time), cols(time)] result;
        result = log(b) - log(a)+ (b - 1) * (log(time) - log(a)) - log( 1 + (time./a).^b) ;
        return result;
    }
}

parameters {
    // SurvivalLogLogistic
    real<lower={{ machine_double_eps }}> sm_loglogis_a;
    real<lower={{ machine_double_eps }}> sm_loglogis_b;
}


transformed parameters {
    // SurvivalLogLogistic
    vector[2] pars_os = [sm_loglogis_a, sm_loglogis_b]';
}

