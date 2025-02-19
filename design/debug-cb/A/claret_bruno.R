

library(cmdstanr)
library(dplyr)
library(ggplot2)
library(bayesplot)


get_sld <- function(t, b, g, c, p) {
    b * exp((g * t) - (p/c) * (1 - exp(-c * t)))
}

pars <- list(
    b = 60,
    g = 0.5,
    c = 0.4,
    p = 0.7,
    sigma = 0.002
)

dat <- tibble(
    t = seq(1, 900, by = 5) / 365,
    sld_mu = get_sld(t, pars$b, pars$g, pars$c, pars$p),
    sld = rnorm(length(t), sld_mu, sld_mu * pars$sigma)
)

ggplot(data = dat, aes(x = t, y = sld)) +
    geom_point() +
    geom_line(aes(y = sld_mu), color = "red")


mod <- cmdstan_model(
    stan_file = here::here("design/debug-cb/A/claret_bruno.stan")
)

stan_data <- list(
    N = nrow(dat),
    values = dat$sld,
    times = dat$t
)

fit <- mod$sample(
    data = stan_data,
    chains = 2,
    parallel_chains = 2,
    refresh = 200,
    iter_warmup = 1000,
    iter_sampling = 2000
)

fit$summary()







