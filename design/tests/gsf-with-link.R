library(dplyr)
library(ggplot2)
library(stringr)
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
    longitudinal = LongitudinalGSF(

        mu_bsld = prior_lognormal(log(70), 5),
        mu_ks = prior_lognormal(log(0.2), 1),
        mu_kg = prior_lognormal(log(0.2), 1),
        mu_phi = prior_beta(2, 2),

        omega_bsld = prior_lognormal(log(0.135), 1),
        omega_ks = prior_lognormal(log(0.15), 1),
        omega_kg = prior_lognormal(log(0.225), 1),
        omega_phi = prior_lognormal(log(0.75), 1),

        sigma = prior_lognormal(log(0.01), 1),

        tilde_bsld = prior_normal(0, 5),
        tilde_ks = prior_normal(0, 2),
        tilde_kg = prior_normal(0, 1),
        tilde_phi = prior_normal(0, 5)
    ),
    survival = SurvivalExponential(),
    link = LinkGSF()
)




# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")



## Prepare data for sampling
jdat <- DataJoint(
    survival = DataSurvival(
        data = dat_os,
        formula = Surv(time, event) ~ cov_cat + cov_cont,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    longitudinal = DataLongitudinal(
        data = dat_lm,
        formula = sld ~ time,
        subject = "pt",
        threshold = 5
    )
)

## Sample from JointModel

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 500,
    iter_warmup = 1000,
    chains = 1,
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



################################
#
# General Diagnostic stuff
#
#



mp@results$aummary(vars)


library(bayesplot)
mcmc_trace(mp@results$draws("lm_gsf_mu_phi[1]"))
mcmc_trace(mp@results$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp@results$draws("lm_gsf_mu_phi[1]"))

mcmc_pairs(mp@results$draws(), vars)
