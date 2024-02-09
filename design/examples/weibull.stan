


data {
    int<lower=1> n;
    int<lower=0> p;
    vector[n] times;
    vector[n] event_fl;
    matrix[n, p] design;
}

parameters {
    vector[p] log_ph_beta;
    real gamma;
}

model {

    real alpha = gamma;
    vector[n] lambda = exp(design * log_ph_beta);
    vector[n] sigma = lambda .^ (-1/alpha);

    // Priors
    log_ph_beta ~ normal(0, 3);
    gamma ~ lognormal(log(1), 1.5);
    
    // Likelihood
    for (i in 1:n) {
        if (event_fl[i] == 1) {
            target += weibull_lpdf(times[i] | alpha, sigma[i]);
        } else {
            target += weibull_lccdf(times[i] | alpha, sigma[i]);
        }
    }
}

generated quantities {
    // Reconstruct the baseline lambda value from the intercept
    // of the design matrix
    real lamba_0 = exp(log_ph_beta[1]);
}


