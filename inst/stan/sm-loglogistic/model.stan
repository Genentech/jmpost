

functions {
    // SurvivalloglogisticPH
    matrix log_h0(matrix time, vector pars_os) {
        real lambda = pars_os[1];
        real p = pars_os[2];
        matrix[rows(time), cols(time)] result;
        result =log(lambda)+log(p)+(p-1)*(log(lambda)+log(time))-log1p(lambda*time^.p) ;
        return result;
    }
}


parameters {
    // SurvivalWeibullPH
    real<lower=0.000000000001> sm_loglogistic_ph_lambda;
    real<lower=0.0000001> sm_loglogistic_ph_p;
}


transformed parameters {
    // SurvivalWeibullPH
    vector[2] pars_os = [sm_loglogistic_ph_lambda, sm_loglogistic_ph_p]';
}



// #' @export
// sim_os_loglogistic <- function(lambda, p){
//     function(time) {
//         c1 <- lambda * p * (lambda * time)^(p - 1)
//         c2 <- 1 + (lambda * time)^p
//         log(c1 / c2)
//     }
// }
