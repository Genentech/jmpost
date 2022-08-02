  real<lower=0> p;
  real<lower=0> lambda; // For the log-logistic baseline hazard.
  <link_parameters>
  vector[p_os_cov_design] beta_os_cov; // Covariate coefficients.
