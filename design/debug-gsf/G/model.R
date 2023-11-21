

set.seed(32130)
library(dplyr)
library(tibble)
library(tidyr)
library(cmdstanr)
library(bayesplot)
library(ggplot2)
library(here)


gsf_sld <- function(time, b, s, g) {
    b * (exp(-s * time) +  exp(g * time) - 1)
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


n <- 200
baseline <- tibble(
    pt = factor(sprintf("pt_%04i", 1:n)),

    mu_b = log(60),
    mu_s = log(0.6),
    mu_g = log(0.2),

    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,

    LB = rnorm(n, mu_b, sigma_b),
    LS = rnorm(n, mu_s, sigma_s),
    LG = rnorm(n, mu_g, sigma_g),

    b = exp(LB),
    s = exp(LS),
    g = exp(LG),

    sigma = 5
)

pdat <- baseline |>
    select(b, g, s) |>
    gather("key", "var")

ggplot(data = pdat, aes(x = var)) +
    geom_density() +
    theme_bw() +
    facet_wrap(~key, scales = "free")



grid_df <- tidyr::expand_grid(
    pt = baseline$pt,
    time = seq(0, 7, by = 0.3)
) |>
    filter(time != 0)

dat_lm <- grid_df |>
    left_join(baseline, by = "pt") |>
    mutate(msld = gsf_sld(time, b, s, g)) |>
    mutate(sld = rnorm(nrow(grid_df), msld, sigma)) |>
    select(pt, b, s, g, sigma, time, msld, sld)


selected_pts <- sample(baseline$pt, 8)
ggplot(
    data = dat_lm |> filter(pt %in% selected_pts),
    aes(x = time, y = sld, group = pt, color = pt)
) +
    geom_point() +
    geom_line() +
    theme_bw()


baseline |> summarise(across(c("b", "g", "s", "sigma"), mean))


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

    LB = rep(log(60), n),
    LS = rep(log(0.6), n),
    LG = rep(log(0.2), n),

    mu_b = 60,
    mu_s = 0.6,
    mu_g = 0.2,

    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,

    sigma = 0.05
)


mod <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "G", "model.stan")),
    exe_file = file.path(here("local", "models", "model_G"))
)
nchains <- 3
fit <- mod$sample(
    data = stan_data,
    chains = nchains,
    parallel_chains = nchains,
    init = lapply(1:nchains, \(...) init_vals),
    refresh = 200,
    iter_warmup = 400,
    iter_sampling = 600
)
baseline |> summarise(across(c("b", "g", "s",  "sigma"), mean))
pars <- c(
    "mu_b", "mu_s", "mu_g",
    "sigma_b", "sigma_s", "sigma_g", "sigma",
    "b_mean", "s_mean", "g_mean"
)
fit$summary(variables = pars)




#################################
#
#  Diagnostics
#

pars <- c(
    "mu_b", "mu_s", "mu_g",
    "sigma_b", "sigma_s", "sigma_g", "sigma",
    "b_mean", "s_mean", "g_mean"
)


lp_cp <- log_posterior(fit)
np_cp <- nuts_params(fit)

samps <- fit$draws()
mcmc_pairs(samps, np = np_cp, pars = pars)
mcmc_trace(samps, pars = pars)




