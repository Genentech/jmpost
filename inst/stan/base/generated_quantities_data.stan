
data {
    //
    // Source - base/generated_quantities_data.stan
    //
    int<lower=1> n_lm_time_grid;         // Number of time points in the grid (lm)
    vector[n_lm_time_grid] lm_time_grid; // Time points grid.
    int<lower=1> n_sm_time_grid;         // Number of time points in the grid (os)
    vector[n_sm_time_grid] sm_time_grid; // Time points grid.
}
