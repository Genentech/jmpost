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
    n_arm = 200,
    times = seq(0, 1000),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_gsf(
        sigma = 0.014,
        mu_s = 0.0055,
        mu_g = 0.0015,
        mu_phi = 0.2,
        mu_b = 60,
        omega_b = 0.4,
        omega_s = 0.4,
        omega_g = 0.4,
        omega_phi = 0.7,
        link_dsld = 0,
        link_ttg = 0,
        link_identity = 0.002
    ),
    os_fun = sim_os_exponential(1/400)
)


## Extract data to individual datasets
dat_os <- jlist$os
mean(dat_os$time)

select_times <- seq(0, 1000, by = 15)

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% select_times) |>
    dplyr::arrange(pt, time) |>
    dplyr::filter(observed)




pnam <- unique(dat_os$pt) |> sample(size = 10)

ggplot(data = dat_lm |> dplyr::filter(pt %in% pnam)) +
    geom_point(aes(x = time, y = sld, col = pt, group = pt)) +
    geom_line(aes(x = time, y = sld, col = pt, group = pt)) +
    theme_bw()


jm <- JointModel(
    longitudinal = LongitudinalGSF(

        mu_bsld = prior_lognormal(log(70), 0.7),
        mu_ks = prior_lognormal(log(0.0015), 0.7),
        mu_kg = prior_lognormal(log(0.0015), 0.7),
        mu_phi = prior_beta(2, 5),

        omega_bsld = prior_lognormal(log(0.4), 0.7),
        omega_ks = prior_lognormal(log(0.4), 0.7),
        omega_kg = prior_lognormal(log(0.4), 0.7),
        omega_phi = prior_lognormal(log(0.4), 0.7),

        sigma = prior_lognormal(log(0.014), 0.7)
    ),
    survival = SurvivalExponential(
        lambda = prior_lognormal(log(1 / 400), 0.7)
    ),
    link = LinkGSF(link_gsf_identity(tau = prior_normal(0.002, 0.2)))
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
        study = "study",
        time_grid = c(1)
    ),
    longitudinal = DataLongitudinal(
        data = dat_lm,
        formula = sld ~ time,
        subject = "pt",
        threshold = 5,
        time_grid = c(1)
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
    exe_file = file.path("local", "models", "gsf-with-link-identity")
)
    




vars <- c(
    "lm_gsf_mu_bsld", "lm_gsf_mu_phi",
    "lm_gsf_mu_kg", "lm_gsf_mu_ks",
    "lm_gsf_omega_bsld", "lm_gsf_omega_kg",
    "lm_gsf_omega_phi", "lm_gsf_omega_ks",
    "lm_gsf_sigma",
    "sm_exp_lambda", "lm_gsf_tau"
)

mp@results$summary(vars)


################################
#
# General Diagnostic stuff
#
#



library(bayesplot)
mcmc_trace(mp@results$draws("lm_gsf_mu_phi[1]"))
mcmc_trace(mp@results$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp@results$draws("lm_gsf_mu_phi[1]"))

mcmc_pairs(mp@results$draws(), vars)
