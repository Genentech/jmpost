library(dplyr)
library(ggplot2)
library(stringr)
library(survival)
library(tidyr)

devtools::document()
devtools::load_all(export_all = FALSE)

# Fully specified model
jm <- JointModel(
    longitudinal_model = LongitudinalRandomSlope(),
    link = LinkRandomSlope(),
    survival_model = SurvivalWeibullPH()
)

# Fit both models but with no link
jm <- JointModel(
    longitudinal_model = LongitudinalRandomSlope(),
    link = LinkNone(),
    survival_model = SurvivalWeibullPH()
)

# Fit survival model only
jm <- JointModel(
    survival_model = SurvivalWeibullPH()
)

# Fit longitudinal model only
jm <- JointModel(
    longitudinal_model = LongitudinalRandomSlope()
)

# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")


## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n = 1000,
    max_time = 2000,
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
        slope = 2,
        z_sigma = 0.2,
        phi = 0
    ),
    os_fun = sim_os_weibull(
        lambda = 1 / 300,
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





###################### Debug Section - Please Ignore


##############################
#
#  Check bayesian models rs parameters are correct
#

slopes <- tibble(
    est = draws_means[grep("lm_rs_rslope", names(draws_means), value = TRUE)],
    actual = dat_lm |> group_by(pt) |> slice(1) |> pull(slope_ind)
)

mean(slopes$actual - slopes$est)
sd(slopes$actual - slopes$est)

ggplot(slopes, aes(x = est, y = actual)) +
    geom_point() +
    geom_abline()








##############################
#
#  Test random effects using MLE approach
#

library(lme4)

mod <- lmer(outcome ~ time + (time -1 | pt), data = dat_lm, REML = F)

fixef(mod)

slopes <- tibble(
    est = ranef(mod)$pt$time + fixef(mod)[["time"]],
    actual = dat_lm |>
        group_by(pt) |>
        slice(1) |> pull(slope_ind)
)

ggplot(slopes, aes(x = est, y = actual )) +
    geom_point() +
    geom_abline()




##############################
#
# Test MLE OS parameters are correct from simulation
#

coxph(data = dat_os, Surv(time, event) ~ cov_cat + cov_cont)

mod <- survreg(Surv(time, event) ~ cov_cat + cov_cont, data = dat_os, dist = "weibull")

c(
    "lambda" = exp(-coef(mod)[["(Intercept)"]]/mod$scale),
    "gamma" = 1/mod$scale,
    -coef(mod)[-1]/mod$scale
)
