
devtools::document()
devtools::load_all(export_all = FALSE)

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

options("jmpost.cache.dir" = file.path("local", "models"))


#### Example 1 - Fully specified model
jm <- JointModel(
    longitudinal = LongitudinalRandomSlope(
        intercept = prior_normal(30, 2),
        slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
        sigma = prior_lognormal(log(3), sigma = 0.5)
    ),
    survival = SurvivalExponential()
)



# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")


## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n = c(200, 250),
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
        sigma = 3,
        slope_mu = c(0.01, 0.03),
        slope_sigma = 0.2,
        phi = 0
    ),
    os_fun = sim_os_exponential(
        lambda = 0.00333  # true value = 1 / 300
    ),
    .debug = TRUE
)


## Extract data to individual datasets
dat_os <- jlist$os
dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 100, 150, 250, 300)) |>
    dplyr::arrange(time, pt)



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

stan_samples <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 400,
    iter_warmup = 400,
    chains = 1,
    parallel_chains = 1
)


pts <- sample(dat_os$pt, 4)
samps <- LongitudinalQuantities(
    stan_samples,
    groups = sample(dat_os$pt, 4)
)
summary(samps)
as.data.frame(samps) |> tibble()
autoplot(samps)




pts <- sample(dat_os$pt, 4)
samps <- SurvivalQuantities(
    stan_samples,
    groups = sample(dat_os$pt, 4)
)
summary(samps)
as.data.frame(samps) |> tibble()
autoplot(samps)





pts <- list(
    "g1" = sample(dat_os$pt, 100),
    "g2" = sample(dat_os$pt, 100)
)
samps <- extractSurvivalQuantities(
    stan_samples,
    patients = pts,
    type = "surv",
    time_grid = c(0, 100, 200)
)
summary(samps)
as.data.frame(samps) |> tibble()
autoplot(samps)




pts <- list(
    "g1" = sample(dat_os$pt, 10),
    "g2" = sample(dat_os$pt, 10)
)
samps <- extractSurvivalQuantities(
    stan_samples,
    patients = pts,
    type = "surv"
)
autoplot(
    samps,
    add_km = TRUE
)

autoplot(
    samps,
    add_wrap = FALSE
)
