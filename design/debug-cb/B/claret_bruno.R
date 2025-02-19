

library(cmdstanr)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(tidyr)


get_sld <- function(t, b, g, c, p) {
    b * exp((g * t) - (p/c) * (1 - exp(-c * t)))
}

pars_mu <- list(
    b = 60,
    g = 0.5,
    c = 0.4,
    p = 0.7,
    sigma = 0.02
)

pars_sigma <- list(
    b = 0.1,
    g = 0.1,
    c = 0.1,
    p = 0.1
)

N <- 120

dat_baseline <- tibble(
    pt = sprintf("pt%05d", 1:N),
    b = exp(rnorm(N, log(pars_mu$b), pars_sigma$b)),
    g = exp(rnorm(N, log(pars_mu$g), pars_sigma$g)),
    c = exp(rnorm(N, log(pars_mu$c), pars_sigma$c)),
    p = exp(rnorm(N, log(pars_mu$p), pars_sigma$p))
)

dat <- tidyr::crossing(
    pt = dat_baseline$pt,
    t = seq(1, 900, length.out = 8) / 365
) |>
    left_join(dat_baseline, by = "pt") |>
    mutate(
        sld_mu = get_sld(t, b, g, c, p),
        sld = rnorm(n(), sld_mu, sld_mu * pars_mu$sigma)
    ) |>
    arrange(pt, t) |>
    mutate(pt = factor(pt))

pdat <- dat |>
    filter(pt %in% sample(dat$pt, 5))


ggplot(data = pdat, aes(x = t, y = sld, group = pt, col = pt)) +
    geom_point() +
    geom_line(aes(y = sld_mu))


mod <- cmdstan_model(
    stan_file = here::here("design/debug-cb/B/claret_bruno.stan")
)

stan_data <- list(
    N_obs = nrow(dat),
    N_pt = N,
    pt_index = as.numeric(dat$pt),
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

fit
fit$summary()







