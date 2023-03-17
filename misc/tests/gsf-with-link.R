library(dplyr)
library(ggplot2)
library(stringr)
library(survival)
library(tidyr)
library(cmdstanr)

# devtools::install_git("https://github.com/stan-dev/cmdstanr")

devtools::document()
devtools::load_all(export_all = FALSE)



#### Example 1 - Fully specified model - using the defaults for everything



## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n_arm = c(80, 80),
    times = seq(0, 4, by = (1/365)/2),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_gsf(
        sigma = 0.003,
        mu_s = c(0.2, 0.25),
        mu_g = c(0.15, 0.2),
        mu_phi = c(0.4, 0.6),
        mu_b = 60,
        eta_b_sigma = 0.5,
        eta_s_sigma = 0.5,
        eta_g_sigma = 0.5,
        eta_phi_sigma = 0.5,
        omega_b = 0.1,
        omega_s = 0.1,
        omega_g = 0.1,
        omega_phi = 0.2,
        link_dsld = 0.02,
        link_ttg = -0.02
    ),
    os_fun = sim_os_weibull(
        lambda = 365 * (1/400),
        gamma = 1
    )
)


## Extract data to individual datasets
dat_os <- jlist$os

select_times <- c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900) * (1 / 365)

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% select_times) |>
    dplyr::arrange(pt, time)




pnam <- unique(dat_os$pt) |> sample(size = 10)

ggplot(data = dat_lm |> dplyr::filter(pt %in% pnam)) +
    geom_point(aes(x = time, y = sld, col =pt, group =pt)) +
    geom_line(aes(x = time, y = sld, col =pt, group =pt)) +
    theme_bw()


jm <- JointModel(
    longitudinal_model = LongitudinalGSF(
        
        mu_bsld = Parameter(prior_lognormal(log(70), 5), init = 70),
        mu_ks = Parameter(prior_lognormal(log(0.2), 1), init = 0.3),
        mu_kg = Parameter(prior_lognormal( log(0.2), 1), init = 0.2),
        mu_phi = Parameter(prior_beta(2, 2), init = 0.2),
        
        omega_bsld = Parameter(prior_lognormal(log(0.135), 1), init = 0.01),
        omega_ks = Parameter(prior_lognormal(log(0.15), 1), init = 0.01),
        omega_kg = Parameter(prior_lognormal(log(0.225), 1), init = 0.01),
        omega_phi = Parameter(prior_lognormal(log(0.75), 1), init = 0.01),
        
        sigma = Parameter(prior_lognormal(log(0.01), 1), init = 0.01),
        
        tilde_bsld = Parameter(prior_normal(0, 5), init = 0.1),
        tilde_ks = Parameter(prior_normal(0, 2), init = 0.1),
        tilde_kg = Parameter(prior_normal(0, 1), init = 0.1),
        tilde_phi = Parameter(prior_normal(0, 5), init = 0.1)
    ),
    survival_model = SurvivalExponential(),
    link = LinkGSF()
)

x <- as.list(jm@inits)
x$lm_gsf_mu_bsld <- c(70)
x$lm_gsf_mu_kg <- c(0.3, 0.3)
x$lm_gsf_mu_phi <- c(0.3, 0.3)
x$lm_gsf_mu_ks <- c(0.3, 0.3)

initial_values <- function() {
    x
}



# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")



## Prepare data for sampling
stan_data <- as_stan_data(dat_os, dat_lm, ~ cov_cat + cov_cont)
stan_data$Times

## Sample from JointModel

mp <- sampleStanModel(
    jm,
    data = stan_data,
    iter_sampling = 500,
    iter_warmup = 1000,
    chains = 1,
    init = initial_values,
    parallel_chains = 1,
    exe_file = file.path("local", "full")
)





vars <- c(
    "lm_gsf_mu_bsld[1]",
    "lm_gsf_mu_phi[1]", "lm_gsf_mu_phi[2]",
    "lm_gsf_mu_kg[1]", "lm_gsf_mu_kg[2]",
    "lm_gsf_mu_ks[1]", "lm_gsf_mu_ks[2]",
    "lm_gsf_sigma",
    "lm_gsf_omega_bsld", "lm_gsf_omega_kg",
    "lm_gsf_omega_phi", "lm_gsf_omega_ks",
    "sm_exp_lambda", "lm_gsf_beta", "lm_gsf_gamma"
)

mp$summary(vars)
library(bayesplot)
mcmc_trace(mp$draws("lm_gsf_mu_phi[1]"))
mcmc_trace(mp$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp$draws("lm_gsf_mu_phi[1]"))

mcmc_pairs(mp$draws(), vars)
