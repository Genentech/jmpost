
functions {
    vector claret_bruno_mu(vector times, real b, real c, real p, real g) {
        vector[rows(times)] values;
        values = b * exp(
            (g .* times) -  (
                (p/c) .* (1 - exp(-c .* times))
            )
        );
        return values;
    }
}


data {
    int <lower=0> N;
    vector[N] values;
    vector[N] times;
}

parameters {
   real <lower=0>  b;
   real <lower=0>  c;
   real <lower=0>  p;
   real <lower=0>  g;
   real <lower=0> sigma;
}

transformed parameters {
    vector[N] mu = claret_bruno_mu(times, b, c, p, g);
}

// pars <- list(
//     b = 60,
//     g = 0.5,
//     c = 0.4,
//     p = 0.7,
//     sigma = 0.004
// )

model {
    b ~ lognormal(log(60), 0.5);
    c ~ lognormal(log(0.5), 0.5);
    p ~ normal(log(0.4), 0.5);
    g ~ lognormal(log(0.7), 0.5);
    sigma ~ lognormal(log(0.004), 0.5);
    values ~ normal(mu, mu * sigma);
}




