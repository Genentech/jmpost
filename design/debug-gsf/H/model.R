

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


x <- rlnorm(1000, log(60), 1)
plot(density(x))


x <- rlnorm(1000, log(0.2), 1)
plot(density(x))


x <- rbeta(1000, 3, 3)
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

    mu_b = log(60),
    mu_s = log(0.6),
    mu_g = log(0.2),
    mu_phi = logit(0.5),

    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,
    sigma_phi = 0.3,

    eta_b = rnorm(n, 0, 1),
    eta_s = rnorm(n, 0, 1),
    eta_g = rnorm(n, 0, 1),
    eta_phi = rnorm(n, 0, 1),

    b = exp(mu_b + eta_b * sigma_b),
    g = exp(mu_g + eta_g * sigma_s),
    s = exp(mu_s + eta_s * sigma_g),
    phi = inv_logit(mu_phi + eta_phi * sigma_phi),

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
    time = seq(0, 6, length.out = 9)
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
    raw_b = rep(0, n),
    raw_s = rep(0, n),
    raw_g = rep(0, n),
    raw_phi = rep(0, n),
    mu_b = log(60),
    mu_s = log(0.6),
    mu_g = log(0.2),
    mu_phi = log(0.5),
    sigma_b = 0.3,
    sigma_s = 0.3,
    sigma_g = 0.3,
    sigma_phi = 0.3,
    sigma = 0.05
)


mod <- cmdstan_model(
    stan_file = file.path(here("design", "debug-gsf", "H", "model.stan")),
    exe_file = file.path(here("local", "models", "model_H"))
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

pars <- c( "sigma", "pop_mean_b", "pop_mean_g", "pop_mean_s")

baseline |> summarise(across(c("b", "g", "s", "phi", "sigma"), mean))
fit$summary(variables = pars)
fit$summary()



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























# A tibble: 4 × 10
  variable      mean  median      sd     mad      q5     q95  rhat ess_bulk
  <chr>        <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>    <dbl>
1 sigma       0.0513  0.0513 0.00110 0.00112  0.0495  0.0531  1.00   1184. 
2 pop_mean_b 63.2    63.1    1.76    1.77    60.6    66.4     1.03     76.6
3 pop_mean_g  0.197   0.197  0.00618 0.00637  0.187   0.207   1.03    157. 
4 pop_mean_s  0.599   0.599  0.0484  0.0500   0.521   0.679   1.00    659. 
# ℹ 1 more variable: ess_tail <dbl>
> fit$summary()
# A tibble: 1,613 × 10
   variable       mean   median      sd     mad       q5      q95  rhat ess_bulk
   <chr>         <dbl>    <dbl>   <dbl>   <dbl>    <dbl>    <dbl> <dbl>    <dbl>
 1 lp__       -3.22e+3 -3.22e+3 28.1    28.7    -3.27e+3 -3.18e+3  1.01    187. 
 2 mu_b        4.09e+0  4.09e+0  0.0269  0.0265  4.05e+0  4.14e+0  1.03     80.4
 3 mu_s       -5.19e-1 -5.16e-1  0.0822  0.0843 -6.57e-1 -3.89e-1  1.00    672. 
 4 mu_g       -1.67e+0 -1.67e+0  0.0328  0.0333 -1.72e+0 -1.62e+0  1.02    180. 
 5 mu_phi     -1.20e-1 -1.20e-1  0.0467  0.0459 -1.97e-1 -4.02e-2  1.00    708. 
 6 sigma_b     3.31e-1  3.29e-1  0.0174  0.0171  3.04e-1  3.61e-1  1.04    118. 
 7 sigma_s     7.48e-2  6.86e-2  0.0373  0.0370  2.35e-2  1.44e-1  1.02    297. 
 8 sigma_g     2.93e-1  2.92e-1  0.0161  0.0162  2.68e-1  3.20e-1  1.02    151. 
 9 sigma_phi   5.11e-1  2.84e-1  0.709   0.261   5.20e-2  1.68e+0  1.00   3256. 
10 raw_b[1]    3.76e-1  3.68e-1  0.228   0.226   1.84e-2  7.44e-1  1.01    455. 