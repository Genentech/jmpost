
data {
    int n;
    array[n] real x;
}

parameters {
   real mu;
   real sigma;
}

model {
   x ~ normal(mu, sigma);
}
