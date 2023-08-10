// Dummy model just to get stan to do a single iteration so we can call
// functions in the "generated quantities" block
data {
    vector[2] null_y_data;
}

parameters {
    real null_y_mean;
}

model {
    null_y_data ~ normal(null_y_mean, 1);
}
