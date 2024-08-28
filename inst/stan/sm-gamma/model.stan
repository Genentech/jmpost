

functions {
    //
    // Source - sm-gamma/model.stan
    //
    matrix log_h0(matrix time, vector pars_os) {
        real k = pars_os[1];
        real theta = pars_os[2];
        matrix[rows(time), cols(time)] result;
        result = (k - 1) .* log(time) - (time ./ theta) -
            (k * log(theta)) - log(tgamma(k) * (1 - gamma_p(k, time ./ theta)));
        return result;
    }
}


parameters {
    //
    // Source - sm-gamma/model.stan
    //
    real<lower={{ machine_double_eps }}> sm_gamma_k;
    real<lower={{ machine_double_eps }}> sm_gamma_theta;
}


transformed parameters {
    //
    // Source - sm-gamma/model.stan
    //
    vector[2] pars_os = [sm_gamma_k, sm_gamma_theta]';
}

