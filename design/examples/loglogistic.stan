


data {
    int<lower=1> n;                          // Number of subjects
    int<lower=0> p;                          // Number of covariates (including intercept)
    vector<lower=0>[n] times;                // Event|Censor times
    array[n] int<lower=0, upper=1> event_fl; // 1=event 0=censor
    matrix[n, p] design;                     // Design matrix
}

transformed data {
    // Assuming that the first term is an intercept column which
    // will conflict with the alpha_0 term so remove it
    matrix[n, p-1] design_reduced;
    if (p > 1 ) {
        design_reduced = design[, 2:p];
    }}

parameters {
    vector[p-1] beta_design;
    real<lower=0> alpha_0;
    real<lower=0> beta_0;
}

transformed parameters {
    vector[n] alpha;
    if (p == 1) {
        alpha = rep_vector(alpha_0, n);
    } else {
        alpha = alpha_0 .* exp(design_reduced * beta_design);
    }

    // Likelihood
    vector[n] log_lik;
    for (i in 1:n) {
        if (event_fl[i] == 1) {
            log_lik[i] = loglogistic_lpdf(times[i] | alpha[i], beta_0);
        } else {
            log_lik[i] = log(1 - loglogistic_cdf(times[i] | alpha[i], beta_0));
        }
    }
}

model {
    // Priors
    beta_design ~ normal(0, 3);
    alpha_0 ~ lognormal(log(2), 1);
    beta_0 ~ lognormal(log(2), 1);
    target += sum(log_lik);
}
