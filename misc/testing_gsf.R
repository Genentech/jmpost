library(dplyr)
library(ggplot2)
library(stringr)
library(survival)
library(tidyr)

devtools::document()
devtools::load_all(export_all = FALSE)




#### Example 1 - Fully specified model - using the defaults for everything

jm <- JointModel(
    longitudinal_model = LongitudinalGSF()
)


# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")


## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n_arm = c(30, 40),
    max_time = 2000,
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_gsf(),
    os_fun = sim_os_weibull(
        lambda = 0.00333,  # 1/300
        gamma = 0.97
    )
)


## Extract data to individual datasets
dat_os <- jlist$os
dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
    dplyr::arrange(time, pt)

# mean(dat_os$time)
# mean(dat_os$event)


## Prepare data for sampling
stan_data <- as_stan_data(dat_os, dat_lm, ~ cov_cat + cov_cont)


## Sample from JointModel

dir.create(path = file.path("local"), showWarnings = FALSE)
mp <- sampleStanModel(
    jm,
    data = stan_data,
    iter_sampling = 1000,
    iter_warmup = 1000,
    chains = 1,
    parallel_chains = 1,
    exe_file = file.path("local", "full")
)


### Example of how to compile model without running it
# model <- compileStanModel(jm, file.path("local", "full_stan"))


### Extract parameters and calculate confidence intervals
draws_means <- mp$draws(format = "df") |>
    gather() |>
    group_by(key) |>
    summarise(
        mean = mean(value),
        sd = sd(value),
        lci = mean - 1.96 * sd,
        uci = mean + 1.96 * sd
     )

#### Get a list of all named parameters
# names(draws_means) |> str_replace("\\[.*\\]", "[]") |> unique()

### Select which parameters we actually care about
### Not all will exist depending on which model was run
vars <- c(
    "sm_weibull_ph_lambda", "sm_weibull_ph_gamma",
    "beta_os_cov[1]", "beta_os_cov[2]", "beta_os_cov[3]",
    "lm_rs_intercept", "lm_rs_slope", "lm_rs_sigma", "link_lm_phi"
)
draws_means |> filter(key %in% vars)



