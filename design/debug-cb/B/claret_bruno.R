

library(cmdstanr)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(tidyr)
library(brms)


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
    chains = 3,
    parallel_chains = 3,
    refresh = 200,
    iter_warmup = 1500,
    iter_sampling = 2000
)

fit
fit$summary()





######################
#
# brms implementation
#
#


# pars_mu <- list(
#     b = 60,
#     g = 0.5,
#     c = 0.4,
#     p = 0.7,
#     sigma = 0.02
# )

bfit <- brm(
    bf(
        value ~ exp(b) * exp(  exp(g) * t - exp(p-c) * (1 - exp(- exp(c) * t))),
        b ~ 1 + (1 | pt),
        g ~ 1 + (1 | pt),
        c ~ 1 + (1 | pt),
        p ~ 1 + (1 | pt),
        nl = TRUE
    ),
    data = dat |> select(pt, value = sld, t),
    prior = c(
        prior("normal(log(60), 0.3)", nlpar = "b"),   # b intercept
        prior("normal(log(0.5), 0.3)", nlpar = "g"),  # g intercept
        prior("normal(log(0.4), 0.3)", nlpar = "c"),  # c intercept
        prior("normal(log(0.7), 0.3)", nlpar = "p"),  # p intercept
        prior("lognormal(log(0.1), 0.3)", nlpar = "b", class = "sd"),  # b random effect sigma
        prior("lognormal(log(0.1), 0.3)", nlpar = "g", class = "sd"),  # g random effect sigma
        prior("lognormal(log(0.1), 0.3)", nlpar = "c", class = "sd"),  # c random effect sigma
        prior("lognormal(log(0.1), 0.3)", nlpar = "p", class = "sd"),  # p random effect sigma
        prior("lognormal(log(0.02), 0.3)", class = "sigma")   # overall sigma
    ),
    warmup = 1500,
    iter = 2500,
    chains = 3,
    cores = 3,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95)
)




#####################
#
#  Debugging 
#
#



# Plot patient profiles
pdat <- dat |> filter(pt %in% sample(dat$pt, 5))

ggplot(data = pdat, aes(x = t, y = sld, group = pt, col = pt)) +
    geom_point() +
    geom_line(aes(y = sld_mu))


# Plottig priors
plot(density(exp(rnorm(5000, log(0.6), 0.2))))


# Plotting Joint Priors
N <- 100000
mu <- rnorm(N, log(0.6), 0.3)
sigma <- exp(rnorm(N, log(0.1), 0.3))
value <- exp(rnorm(N, mu, sigma))

pdat <- tibble(
    mu = mu,
    sigma = sigma,
    value = value
)

ggplot(data = pdat, aes(x =value, y = mu)) +
    geom_bin2d(bins = 300)


