
data {
    vector[2] null_y_data;
    array[2] int dim_surv;
    array[2] int dim_haz;
    array[2] int dim_y_fit;
    matrix[dim_surv[1], dim_surv[2]] ret_log_surv;
    matrix[dim_haz[1], dim_haz[2]] ret_log_haz;
    matrix[dim_y_fit[1], dim_y_fit[2]] ret_y_fit;
}

parameters {
    real null_y_mean;
}

model {
    null_y_data ~ normal(null_y_mean, 1);
}

generated quantities {
    matrix[dim_surv[1], dim_surv[2]] log_surv_fit_at_time_grid = ret_log_surv;
    matrix[dim_haz[1], dim_haz[2]] log_haz_fit_at_time_grid = ret_log_haz;
    matrix[dim_y_fit[1], dim_y_fit[2]] y_fit_at_time_grid = ret_y_fit;
}
