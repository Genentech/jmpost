

data {
    int<lower=1> n;
    int<lower=0> p;
    vector[n] times;
    vector[n] event_fl;
    matrix[n, p] design;
}

transformed data {
    // To make life easier for the user we manually derive the indices
    // of the event and censoring times (this would be easier to implement
    // on the R side but would require more code from the user).
    
    // First we work out how many censoring and events there are each so that
    // we can construct index vectors of the correct size
    int n_event = 0;
    for (i in 1:n) {
        if (event_fl[i] == 1) {
            n_event += 1;
        }
    }
    int n_censor = n - n_event;

    // Now we construct the index vectors
    array[n_event] int event_idx;
    array[n_censor] int censor_idx;
    int j = 1;
    int k = 1;
    for (i in 1:n) {
        if (event_fl[i] == 1) {
            event_idx[j] = i;
            j+=1;
        } else {
            censor_idx[k] = i;
            k+=1;
        }
    }
}

parameters {
    vector[p] beta;
    // real<lower = 0> lambda;  // Lambda is represented via exp(intercept) term of the design matrix
    real<lower = 0> gamma;
}


model {
    // Stans in built distributions use the AFT parameterisation of the Weibull distribution
    // As such we need to convert our PH parameters to the AFT parameters
    real d_alpha;
    vector[n] d_beta;
    d_beta = exp(design * beta)^(-1/gamma);
    d_alpha = gamma;
    
    // Priors
    beta ~ normal(0, 2);
    gamma ~ normal(1, 0.5);
    
    // Likelihood
    target += weibull_lpdf(times[event_idx] | d_alpha, d_beta[event_idx]);
    target += weibull_lccdf(times[censor_idx] | d_alpha, d_beta[censor_idx]);
}

generated quantities {
    // Reconstruct the baseline lambda value from the intercept
    // of the design matrix
    real lambda_0 = exp(beta[1]);
}


