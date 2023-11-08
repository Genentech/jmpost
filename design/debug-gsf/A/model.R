


library(dplyr)
library(tibble)
library(tidyr)
library(cmdstanr)
library(bayesplot)

gsf_sld <- function(time, b, s, g, phi) {
    b * (phi * exp(-s * time) + (1 - phi) * exp(g * time))
}


n <- 200
baseline <- tibble(
    pt = factor(sprintf("pt_%04i", 1:n)),
    bsld = rnorm(n, 60, 10),
    g = 0.2,
    s = 0.3,
    phi = 0.4,
    sigma = 0.01
)

grid_df <- tidyr::expand_grid(
    pt = baseline$pt,
    time = seq(0, 4, by = 0.3)
) |>
    filter(time != 0)

dat_lm <- grid_df |>
    left_join(baseline, by = "pt") |>
    mutate(msld = gsf_sld(time, bsld, s, g, phi)) |>
    mutate(sld = rnorm(nrow(grid_df), msld, msld * sigma))



stan_data <- list(
    N = nrow(dat_lm),
    Nind = nrow(baseline),
    ind_index = as.numeric(dat_lm$pt),
    bsld = baseline$bsld,
    Yobs = dat_lm$sld,
    Tobs = dat_lm$time
)
init_vals <- list(
    ks = 0.3,
    kg = 0.2,
    phi = 0.4,
    sigma = 0.01
)


mod <- cmdstan_model(
    stan_file = "./local/debug-gsf/A/model.stan"
)
fit <- mod$sample(
    data = stan_data,
    chains = 1,
    parallel_chains = 1,
    init = list(init_vals),
    refresh = 200,
    iter_warmup = 200,
    iter_sampling = 400
)
fit

lp_cp <- log_posterior(fit)
np_cp <- nuts_params(fit)

samps <- fit$draws()
mcmc_pairs(samps, np = np_cp)
mcmc_trace(samps)






x <- rlnorm(500, log(0.4), 0.5)
plot(density(x))


x <- rbeta(500, 3, 3)
plot(density(x))
