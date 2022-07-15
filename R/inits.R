
long_inits <- list(
    mean_mu_ks = 60,
    mean_mu_kg = 0.7,
    mean_mu_phi = 0.1,
    sd_mu_ks = 0.1,
    sd_mu_kg = 0.1,
    sd_mu_phi = 0.1,
    mu_bsld = function(x) rep(60, x@data$n_studies),
    mu_ks = function(x) rep(3, x@data$n_arms),
    mu_kg = function(x) rep(.7, x@data$n_arms),
    mu_phi = function(x) rep(.1, x@data$n_arms),
    omega_bsld = 0.1,
    omega_ks = 0.1,
    omega_kg = 0.1,
    omega_phi = 0.1,
    sigma = 0.18,
    eta_tilde_bsld = function(x) rep(0, x@data$Nind),
    eta_tilde_ks = function(x) rep(0, x@data$Nind),
    eta_tilde_kg = function(x) rep(0, x@data$Nind),
    eta_tilde_phi = function(x) rep(0, x@data$Nind)
)


os_inits <- list(
    lambda = 2.8,
    p = 1,
    beta_os_cov = function(x) rep(0, ncol(x@data$os_cov_design))
)

