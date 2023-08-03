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
    lm_fun = sim_lm_sf(
        sigma = 0.003,
        mu_s = c(0.2, 0.25),
        mu_g = c(0.15, 0.2),
        mu_b = 60,
        omega_b = 0.1,
        omega_s = 0.1,
        omega_g = 0.1
    ),
    os_fun = sim_os_weibull(
        lambda = 365 * (1/400),
        gamma = 1
    )
)


## Extract data to individual datasets
dat_os <- jlist$os

select_times <- c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900) * (1 / 365)
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
    longitudinal = LongitudinalSF(

        mu_bsld = prior_lognormal(log(60), 2, init = 60),
        mu_ks = prior_lognormal(log(0.2), 0.1, init = 0.2),
        mu_kg = prior_lognormal(log(0.2), 0.1, init = 0.2),

        omega_bsld = prior_lognormal(log(0.1), 0.5, init = 0.1),
        omega_ks = prior_lognormal(log(0.1), 0.5, init = 0.1),
        omega_kg = prior_lognormal(log(0.1), 0.5, init = 0.1),

        sigma = prior_lognormal(log(0.03), 0.1, init = 0.03)
    )
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
    exe_file = file.path("local", "full")
)



vars <- c(
    "lm_gsf_mu_bsld",     # 60
    "lm_gsf_mu_kg",       # 0.15   0.2
    "lm_gsf_mu_ks",       # 0.2    0.25
    "lm_gsf_sigma",       # 0.003
    "lm_gsf_omega_bsld",  # 0.1
    "lm_gsf_omega_kg",    # 0.1
    "lm_gsf_omega_ks"     # 0.1
)


mp@results$summary(vars)




################################
#
# General Diagnostic stuff
#
#



library(bayesplot)
mcmc_trace(mp@results$draws("lm_gsf_mu_bsld[1]"))
mcmc_hist(mp@results$draws("lm_gsf_mu_bsld[1]"))

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






devtools::document()
devtools::load_all()
jm <- JointModel(
    survival_model = SurvivalWeibullPH()
)
mp <- sampleStanModel(
    jm,
    data = stan_data,
    iter_sampling = 500,
    iter_warmup = 1000,
    chains = 1,
    parallel_chains = 1,
    exe_file = file.path("local", "full")
)
