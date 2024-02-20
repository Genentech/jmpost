

functions {
    // Stan's in-built weibull functions are for the AFT formulation
    // so instead we define here the PH formulation
    real weibull_ph_lpdf (real t, real lambda, real gamma) {
        return log(lambda) + log(gamma) + (gamma -1)* log(t) -lambda * t^gamma;
    }
    real weibull_ph_lccdf (real t, real lambda, real gamma) {
        return -lambda * t^gamma;
    }
}


data {
    int<lower=1> n;                          // Number of subjects
    int<lower=0> p;                          // Number of covariates (including intercept)
    vector<lower=0>[n] times;                // Event|Censor times
    array[n] int<lower=0, upper=1> event_fl; // 1=event 0=censor
    matrix[n, p] design;                     // Design matrix
}

transformed data {
    // Assuming that the first term is an intercept column which
    // will conflict with the lambda term so remove it
    matrix[n, p-1] design_reduced = design[, 2:p];
}

parameters {
    vector[p-1] beta;
    real<lower=0> lambda_0;
    real<lower=0> gamma_0;
}

transformed parameters {
    vector[n] log_lik;
    vector[n] lambda = lambda_0 .* exp(design_reduced * beta);
    // Likelihood
    for (i in 1:n) {
        if (event_fl[i] == 1) {
            log_lik[i] = weibull_ph_lpdf(times[i] | lambda[i], gamma_0);
        } else {
            log_lik[i] = weibull_ph_lccdf(times[i] | lambda[i], gamma_0);
        }
    }
}

model {
    // Priors
    beta ~ normal(0, 3);
    gamma_0 ~ lognormal(log(1), 1.5);
    lambda_0 ~ lognormal(log(0.05), 1.5);
    target += sum(log_lik);
}
