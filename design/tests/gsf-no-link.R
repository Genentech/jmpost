library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(cmdstanr)

# devtools::install_git("https://github.com/stan-dev/cmdstanr")

devtools::document()
devtools::load_all(export_all = FALSE)

options("jmpost.cache.dir" = file.path("local", "models"))

#### Example 1 - Fully specified model - using the defaults for everything



## Generate Test data with known parameters
## (based on internal data, time in years)
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
        sigma = 0.005,
        mu_s = c(0.25, 0.35),
        mu_g = c(0.15, 0.25),
        mu_phi = c(0.4, 0.6),
        mu_b = 60,
        omega_b = 0.1,
        omega_s = 0.1,
        omega_g = 0.1,
        omega_phi = 0.2
    ),
    os_fun = sim_os_exponential(
        lambda = 1 / (730 / 365)
    )
)






## Generate Test data with known parameters
## (based on Kerioui  time in days)
jlist <- simulate_joint_data(
    n_arm = c(80),
    times = seq(0, 900),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_gsf(
        sigma = 0.025,
        mu_s = c(0.007),
        mu_g = c(0.001),
        mu_phi = c(0.2),
        mu_b = 60,
        omega_b = 0.51,
        omega_s = 0.51,
        omega_g = 0.51,
        omega_phi = 0.51
    ),
    os_fun = sim_os_exponential(
        lambda = 1 / 400
    )
)



## Extract data to individual datasets
dat_os <- jlist$os

select_times <- sample(dat_os$time, 30)
# select_times <- seq(1, 2000, by = 30)

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% select_times) |>
    dplyr::arrange(time, pt) |>
    dplyr::mutate(time = time)

# mean(dat_os$time)
# mean(dat_os$event)


pnam <- unique(dat_os$pt) |> sample(size = 10)

ggplot(data = dat_lm |> dplyr::filter(pt %in% pnam)) +
    geom_point(aes(x = time, y = sld, col =pt, group =pt)) +
    geom_line(aes(x = time, y = sld, col =pt, group =pt)) +
    theme_bw()



jm <- JointModel(
    longitudinal = LongitudinalGSF(

        mu_bsld = prior_lognormal(log(60), 0.6),
        mu_ks = prior_lognormal(log(0.007), 0.6),
        mu_kg = prior_lognormal(log(0.001), 0.6),
        mu_phi = prior_beta(7, 10),

        omega_bsld = prior_lognormal(log(0.5), 0.6),
        omega_ks = prior_lognormal(log(0.5), 0.6),
        omega_kg = prior_lognormal(log(0.5), 0.6),
        omega_phi = prior_lognormal(log(0.5), 0.6),

        sigma = prior_lognormal(log(0.03), 0.6)
    )
)





# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")



## Prepare data for sampling
jdat <- DataJoint(
    subject = DataSubject(
        data = dat_os,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    longitudinal = DataLongitudinal(
        data = dat_lm,
        formula = sld ~ time,
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
    parallel_chains = 1
)



vars <- c(
    "lm_gsf_mu_bsld",     # 60
    "lm_gsf_mu_phi",      # 0.4    0.6
    "lm_gsf_mu_kg",       # 0.15   0.2
    "lm_gsf_mu_ks",       # 0.2    0.25
    "lm_gsf_sigma",       # 0.003
    "lm_gsf_omega_bsld",  # 0.1
    "lm_gsf_omega_kg",    # 0.1
    "lm_gsf_omega_phi",   # 0.1
    "lm_gsf_omega_ks"     # 0.1
)



mp@results$summary(vars)



################################
#
# General Diagnostic stuff
#
#



library(bayesplot)
mcmc_trace(mp$draws("lm_gsf_mu_phi[1]"))
mcmc_trace(mp$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp$draws("lm_gsf_mu_phi[1]"))

mcmc_pairs(mp$draws(), vars)

### Extract parameters and calculate confidence intervals
draws_means <- mp$draws(format = "df") |>
    gather() |>
    group_by(key) |>
    summarise(
        mean = mean(value),
        sd = sd(value),
        lci = quantile(value, 0.025),
        uci = quantile(value, 0.975)
     )


#### Get a list of all named parameters
# draws_means$key |> str_replace("\\[.*\\]", "[]") |> unique()

draws_means |> filter(key == "log_lik[42]")


draws_means |>
    filter(key %in% vars) |>
    mutate(key = factor(key, levels = vars)) |>
    arrange(key)


