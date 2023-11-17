

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

logit <- binomial()$linkfun
inv_logit <- binomial()$linkinv

n <- 200
baseline <- tibble(
    pt = factor(sprintf("pt_%04i", 1:n)),

    mu_b = 60,
    mu_s = 0.6,
    mu_g = 0.2,
    mu_phi = 0.5,

    eta_b = rnorm(n, 0, 0.5),
    eta_s = rnorm(n, 0, 0.3),
    eta_g = rnorm(n, 0, 0.3),
    eta_phi = rnorm(n, 0, 0.4),

    b = exp(log(mu_b) + eta_b),
    g = exp(log(mu_g) + eta_g),
    s = exp(log(mu_s) + eta_s),
    phi = inv_logit(logit(mu_phi) + eta_phi),

    sigma = 0.05
)

pdat <- baseline |>
    select(b, g, s, phi) |>
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


baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))


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
    eta_b = rep(0, n),
    eta_s = rep(0, n),
    eta_g = rep(0, n),
    eta_phi = rep(0, n),
    mu_b = 60,
    mu_s = 0.6,
    mu_g = 0.2,
    mu_phi = 0.5,
    sigma = 0.05
)


mod <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "E", "model.stan")),
    exe_file = file.path(here("local", "models", "model_E"))
)
nchains <- 3
fit <- mod$sample(
    data = stan_data,
    chains = nchains,
    parallel_chains = nchains,
    init = lapply(1:nchains, \(...) init_vals),
    refresh = 200,
    iter_warmup = 300,
    iter_sampling = 500
)
baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))
pars <- c(
    "mu_b", "mu_s", "mu_g", "mu_phi", "sigma"
)
fit$summary(variables = pars)




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











