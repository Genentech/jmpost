

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
#  Checks
#

x <- rlnorm(1000, log(60), 1)
plot(density(x))

x <- rlnorm(1000, log(60), 0.5)
plot(density(x))



x <- rlnorm(500, log(100), 0.2)
plot(density(x))


x <- rbeta(500, 3, 3)
plot(density(x))


x <- rlnorm(1000, log(0.6), 0.4)
plot(density(x))


x <- rbeta(1000, 3, 3)
plot(density(x))
x <- rlnorm(1000, log(3), 0.4)
plot(density(x))


#################################
#
#  Setup test data
#


n <- 200
baseline <- tibble(
    pt = factor(sprintf("pt_%04i", 1:n)),

    mu_b = 60,
    mu_s = 0.6,
    mu_g = 0.25,

    sigma_b = 0.4,
    sigma_s = 0.2,
    sigma_g = 0.2,

    a_phi = 2,
    b_phi = 2,

    b = rlnorm(n, log(mu_b), sigma_b),
    g = rlnorm(n, log(mu_g), sigma_g),
    s = rlnorm(n, log(mu_s), sigma_s),
    phi = rbeta(n, a_phi, b_phi),

    sigma = 0.05
)

grid_df <- tidyr::expand_grid(
    pt = baseline$pt,
    time = seq(0, 7, by = 0.3)
) |>
    filter(time != 0)

dat_lm <- grid_df |>
    left_join(baseline, by = "pt") |>
    mutate(msld = gsf_sld(time, b, s, g, phi)) |>
    mutate(sld = rnorm(nrow(grid_df), msld, msld * sigma))


selected_pts <- sample(baseline$pt, 8)
ggplot(
    data = dat_lm |> filter(pt %in% selected_pts),
    aes(x = time, y = sld, group = pt, color = pt)
) +
    geom_point() +
    geom_line() +
    theme_bw()




#################################
#
#  Model fitting
#

stan_data <- list(
    N = nrow(dat_lm),
    Nind = nrow(baseline),
    ind_index = as.numeric(dat_lm$pt),
    Yobs = dat_lm$sld,
    Tobs = dat_lm$time
)
init_vals <- list(
    ind_b = rep(60, n),
    ind_s = rep(0.6, n),
    ind_g = rep(0.25, n),
    ind_phi = rep(0.5, n),

    mu_b = 80,
    mu_s = 0.5,
    mu_g = 0.2,

    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,

    a_phi = 2,
    b_phi = 2,
    sigma = 0.05
)


mod <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "F", "model.stan")),
    exe_file = file.path(here("local", "models", "model_F"))
)
nchains <- 3
fit <- mod$sample(
    data = stan_data,
    chains = nchains,
    parallel_chains = nchains,
    init = lapply(1:nchains, \(...) init_vals),
    refresh = 200,
    iter_warmup = 500,
    iter_sampling = 1000
)
baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))
pars <- c(
    "mu_b", "mu_s", "mu_g", "a_phi", "b_phi",
    "sigma", "b_mean", "s_mean", "g_mean", "phi_mean"
)
fit$summary(variables = pars)




#################################
#
#  Diagnostics
#


pars <- c(
    "mu_b", "mu_s", "mu_g", "a_phi", "b_phi",
    "sigma"
)

lp_cp <- log_posterior(fit)
np_cp <- nuts_params(fit)

samps <- fit$draws()
mcmc_pairs(samps, np = np_cp, pars = pars)
mcmc_trace(samps, pars = pars)

mcmc_acf(samps, pars =pars)



