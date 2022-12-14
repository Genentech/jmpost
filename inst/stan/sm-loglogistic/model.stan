
// TODO - This code is 100% wrong

// functions {
//     // Weibull log hazard distribution
//     matrix log_h0(matrix time, row_vector pars_os) {
//         real lambda = pars_os[1];
//         real gamma = pars_os[2];
//         matrix[rows(time), cols(time)] result;
//         result = log(lambda) + log(gamma) + (gamma - 1) * log(time);
//         return result;
//     }
// }


// parameters {
//     real<lower=0> lambda;
//     real<lower=0> gamma;
// }


// transformed parameters {
//     row_vector[2] pars_os = [lambda, gamma];
// }

// // TODO - Remove and make user inputs
// model {
//     p ~ gamma(2, 0.5);
//     1 / lambda ~ lognormal(0, 5);
// }


