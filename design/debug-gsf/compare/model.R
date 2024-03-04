

set.seed(32130)
library(dplyr)
library(tibble)
library(tidyr)
library(cmdstanr)
library(bayesplot)
library(ggplot2)
library(here)


gsf_sld <- function(time, b, s, g, phi) {
    b * (phi * exp(-s * time) + (1 - phi) * exp(g * time))
}


#################################
#
#  Check Prior densitys
#


x <- rlnorm(1000, log(60), 0.05)
plot(density(x))


x <- rlnorm(500, log(0.4), 0.6)
plot(density(x))


x <- rbeta(500, 3, 3)
plot(density(x))





#################################
#
#  Setup test data
#

n <- 50
baseline <- tibble(
    pt = factor(sprintf("pt_%04i", 1:n)),
    b = rlnorm(n, log(70), 0.6),
    s = rlnorm(n, log(0.6), 0.6),
    g = rlnorm(n, log(0.2), 0.6),
    phi = rbeta(n, 2, 2),
    sigma = 0.08
)


grid_df <- tidyr::expand_grid(
    pt = baseline$pt,
    time = seq(0, 5, length.out = 4)
)

dat_lm <- grid_df |>
    left_join(baseline, by = "pt") |>
    mutate(msld = gsf_sld(time, b, s, g, phi)) |>
    mutate(sld = rnorm(nrow(grid_df), msld, msld * sigma))


#################################
#
#  Data Diagnositics (does it look sensible)
#


pdat <- baseline |>
    select(b, g, s, phi) |>
    gather("key", "var")

ggplot(data = pdat, aes(x = var)) +
    geom_density() +
    theme_bw() +
    facet_wrap(~key, scales = "free")

selected_pts <- sample(baseline$pt, 8)
ggplot(
    data = dat_lm |> filter(pt %in% selected_pts),
    aes(x = time, y = sld, group = pt, color = pt)
) +
    geom_point() +
    geom_line() +
    theme_bw()


baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))


#################################
#
#  Model fitting - Common Settings
#


get_samples <- function(mod, init_vals) {
    stan_data <- list(
        N = nrow(dat_lm),
        Nind = nrow(baseline),
        ind_index = as.numeric(dat_lm$pt),
        Yobs = dat_lm$sld,
        Tobs = dat_lm$time
    )
    nchains <- 3
    
    mod$sample(
        data = stan_data,
        chains = nchains,
        parallel_chains = nchains,
        refresh = 200,
        iter_warmup = 600,
        init = lapply(1:nchains, \(...) init_vals),
        iter_sampling = 600
    )
}



#################################
#
#  Model fitting - Centralised
#


init_vals <- list(
    ind_b = rep(60, n),
    ind_s = rep(0.6, n),
    ind_g = rep(0.2, n),
    ind_phi = rep(0.5, n),
    mu_b = 60,
    mu_s = 0.6,
    mu_g = 0.2,
    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,
    a_phi = 2,
    b_phi = 2,
    sigma = 0.05
)


mod_central <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "compare", "mod_central.stan")),
    exe_file = file.path(here("local", "models", "mod_central"))
)

fit_central <- get_samples(mod_central, init_vals)

pars_cent <- c( "sigma", "pop_mean_b", "pop_mean_g", "pop_mean_s")

baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))
fit_central$summary(variables = pars_cent)






#################################
#
#  Model fitting - De-Centralised  no-log
#


init_vals <- list(
    raw_b = rep(0, n),
    raw_s = rep(0, n),
    raw_g = rep(0, n),
    mu_b = log(60),
    mu_s = log(0.6),
    mu_g = log(0.2),
    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,
    ind_phi = rep(0.5, n),
    a_phi = 2,
    b_phi = 2,
    sigma = 0.05
)


mod_dcent_nl <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "compare", "mod_dcent_nl.stan")),
    exe_file = file.path(here("local", "models", "mod_dcent_nl"))
)

fit_dcent_nl <- get_samples(mod_dcent_nl, init_vals)

pars_dcent_nl <- c("sigma", "pop_mean_b", "pop_mean_g", "pop_mean_s")

baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))
fit_dcent_nl$summary(variables = pars_dcent_nl)

fit_dcent_nl




#################################
#
#  Model fitting - De-Centralised  log
#


init_vals <- list(
    raw_b = rep(0, n),
    raw_s = rep(0, n),
    raw_g = rep(0, n),
    mu_b = 60,
    mu_s = 0.6,
    mu_g = 0.2,
    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,
    ind_phi = rep(0.5, n),
    a_phi = 2,
    b_phi = 2,
    sigma = 0.05
)


mod_dcent_lg <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "compare", "mod_dcent_lg.stan")),
    exe_file = file.path(here("local", "models", "mod_dcent_lg"))
)

fit_dcent_lg <- get_samples(mod_dcent_lg, init_vals)

pars_dcent_lg <- c("sigma", "pop_mean_b", "pop_mean_g", "pop_mean_s")

baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))
fit_dcent_lg$summary(variables = pars_dcent_lg)






# 12 seconds
# ess ~100

#################################
#
#  Diagnostics
#

pars <- c(
    "mu_b", "mu_s", "mu_g", "mu_phi"
)


lp_cp <- log_posterior(fit)
np_cp <- nuts_params(fit)

samps <- fit$draws()
mcmc_pairs(samps, np = np_cp, pars = pars)
mcmc_trace(samps, pars = pars)
















#################################
#
#  Applying model to package sim data
#

devtools::load_all(export = FALSE, path = "../../..")

set.seed(4513)
jlist <- simulate_joint_data(
    .debug = TRUE,
    design = list(
        SimGroup(50, "Arm-A", "Study-X")
    ),
    times = seq(0, 3, by = (1/365)/2),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_gsf(
        sigma = 0.01,
        mu_s = c(0.6),
        mu_g = c(0.3),
        mu_b = 60,
        omega_b = 0.2,
        omega_s = 0.2,
        omega_g = 0.2,
        a_phi = 6,
        b_phi = 8
    ),
    os_fun = sim_os_exponential(
        lambda = 1 / (400 / 365)
    )
)

set.seed(333)
select_times <- sample(jlist$os$time, 5)
# select_times <- seq(1, 2000, by = 30)

dlm <- jlist$lm |>
    dplyr::filter(time %in% select_times) |>
    dplyr::arrange(time, pt) |>
    dplyr::mutate(time = time)


dlm |>
    summarise(across(c("psi_b", "psi_g", "psi_s", "psi_phi"), mean))


mod_dcent_nl <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "compare", "mod_dcent_nl.stan")),
    exe_file = file.path(here("local", "models", "mod_dcent_nl"))
)

init_vals <- list(
    raw_b = rep(0, length(unique(dlm$pt))),
    raw_s = rep(0, length(unique(dlm$pt))),
    raw_g = rep(0, length(unique(dlm$pt))),
    mu_b = log(60),
    mu_s = log(0.6),
    mu_g = log(0.2),
    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,
    ind_phi = rep(0.5, length(unique(dlm$pt))),
    a_phi = 2,
    b_phi = 2,
    sigma = 0.05
)


stan_data <- list(
    N = nrow(dlm),
    Nind = length(unique(dlm$pt)),
    ind_index = as.numeric(factor(dlm$pt)),
    Yobs = dlm$sld,
    Tobs = dlm$time
)
nchains <- 3

res <- mod_dcent_nl$sample(
    data = stan_data,
    chains = nchains,
    parallel_chains = nchains,
    refresh = 200,
    iter_warmup = 4000,
    iter_sampling = 10000,
    init = lapply(1:nchains, \(...) init_vals),
)

res
res$metadata()




fit_dcent_nl <- get_samples(mod_dcent_nl, init_vals)

pars_dcent_nl <- c("sigma", "pop_mean_b", "pop_mean_g", "pop_mean_s")

baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))



