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
    n_arm = c(300),
    times = seq(0, 900),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_sf(
       sigma = 0.025,
        mu_s = c(0.007),
        mu_g = c(0.001),
        mu_b = 60,
        omega_b = 0.51,
        omega_s = 0.51,
        omega_g = 0.51,
    ),
    os_fun = sim_os_exponential(
        lambda = 1/400
    )
)


## Extract data to individual datasets
dat_os <- jlist$os

select_times <- seq(0, 600, by = 50)
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

        mu_bsld = prior_lognormal(log(60), 0.7),
        mu_ks = prior_lognormal(log(0.007), 0.7),
        mu_kg = prior_lognormal(log(0.001), 0.7),

        omega_bsld = prior_lognormal(log(0.5), 0.7),
        omega_ks = prior_lognormal(log(0.5), 0.7),
        omega_kg = prior_lognormal(log(0.5), 0.7),

        sigma = prior_lognormal(log(0.03), 0.7)
    )
)





# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/ebug.stan")



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
    iter_sampling = 1500,
    iter_warmup = 2000,
    chains = 2,
    parallel_chains = 2,
    exe_file = file.path("local", "full")
)



vars <- c(
    "lm_sf_mu_bsld",     # 60
    "lm_sf_mu_kg",       # 0.15   0.2
    "lm_sf_mu_ks",       # 0.2    0.25
    "lm_sf_sigma",       # 0.003
    "lm_sf_omega_bsld",  # 0.1
    "lm_sf_omega_kg",    # 0.1
    "lm_sf_omega_ks"     # 0.1
)


vars <- c(
    "lm_sf_mu_bsld",     # 60
    "lm_sf_mu_kg",       # 0.15   0.2
    "lm_sf_mu_ks"       # 0.2    0.25
)


mp@results$summary(vars)




################################
#
# General Diagnostic stuff
#
#



library(bayesplot)
mcmc_trace(mp@results$draws("lm_sf_mu_bsld[1]"))
mcmc_hist(mp@results$draws("lm_sf_mu_bsld[1]"))

mcmc_pairs(mp@results$draws()[, , c(2:8)])

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
