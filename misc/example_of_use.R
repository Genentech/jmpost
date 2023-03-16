library(dplyr)
library(ggplot2)
library(stringr)
library(survival)
library(tidyr)

devtools::document()
devtools::load_all(export_all = FALSE)




#### Example 1 - Fully specified model - using the defaults for everything

jm <- JointModel(
    longitudinal_model = LongitudinalRandomSlope(),
    link = LinkRandomSlope(),
    survival_model = SurvivalWeibullPH()
)



### Example 2 - Manually specify priors - Fit models independently (no link)

lm <- LongitudinalRandomSlope(
    intercept = prior_normal(40, 5),                     # Just prior
    slope_mu = Parameter(prior_normal(10, 2), init = 30) # Prior and init
)


sm <- SurvivalWeibullPH(
    lambda = prior_gamma(0.2, 0.5)
)

jm <- JointModel(
    link = LinkNone(),
    longitudinal_model = lm,
    survival_model = sm
)


### Example 3 - Specify survival model only

jm <- JointModel(
    survival_model = SurvivalWeibullPH()
)


### Example 4 - Specify longitudinal model only

jm <- JointModel(
    longitudinal_model = LongitudinalRandomSlope()
)






### Example of actually running the model

jm <- JointModel(
    longitudinal_model = LongitudinalRandomSlope()
)


# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")


## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n_arm = c(400, 400),
    times = 1:2000,
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_random_slope(
        intercept = 30,
        slope_mu = c(1, 2),
        slope_sigma = 0.2,
        sigma = 3,
        phi = 0
    ),
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

vars <- c(
    "lm_rs_intercept",
    "lm_rs_slope_mu",
    "lm_rs_slope_sigma",
    "lm_rs_sigma"
)


mp$summary(vars)
