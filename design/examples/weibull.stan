

functions {
    real weibull_ph_lpdf (real t, real lambda, real gamma) {
        return log(lambda) + log(gamma) + (gamma -1)* log(t) -lambda * t^gamma;
    }
    real weibull_ph_lccdf (real t, real lambda, real gamma) {
        return -lambda * t^gamma;
    }
}


data {
    int<lower=1> n;
    int<lower=0> p;
    vector<lower=0>[n] times;
    array[n] int<lower=0, upper=1> event_fl;
    matrix[n, p] design;
}

parameters {
    vector[p] beta;
    real<lower=0> gamma;
}

model {

    vector[n] lambda = exp(design * beta);

    // Priors
    beta ~ normal(0, 3);
    gamma ~ lognormal(log(1), 1.5);
    
    // Likelihood
    for (i in 1:n) {
        if (event_fl[i] == 1) {
            target += weibull_ph_lpdf(times[i] | lambda[i], gamma);
        } else {
            target += weibull_ph_lccdf(times[i] | lambda[i], gamma);
        }
    }
}

generated quantities {
    // Reconstruct the baseline lambda value from the intercept
    // of the design matrix
    real lamba_0 = exp(beta[1]);
}


