library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

devtools::document()
devtools::load_all(export_all = FALSE)

options("jmpost.cache_dir" = file.path("local", "models"))


#### Example 1 - Fully specified model
jm <- JointModel(
    longitudinal = LongitudinalRandomSlope(
        intercept = prior_normal(30, 2),
        slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
        sigma = prior_lognormal(log(3), sigma = 0.5)
    )
)



# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")


## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n = c(400, 400),
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
        slope_mu = c(1, 3),
        slope_sigma = 0.2,
        phi = 0
    ),
    os_fun = sim_os_exponential(
        lambda = 0.00333  # 1 / 300
    ),
    .debug = TRUE
)


## Extract data to individual datasets
dat_os <- jlist$os
dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
    dplyr::arrange(time, pt)


## Prepare data for sampling
jdat <- jdat <- DataJoint(
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

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 1000,
    iter_warmup = 1000,
    chains = 1,
    parallel_chains = 1
)


### Select which parameters we actually care about
### Not all will exist depending on which model was run
vars <- c(
    "lm_rs_intercept",       # 30
    "lm_rs_slope_mu",        # 1 , 3
    "lm_rs_slope_sigma",     # 0.2
    "lm_rs_sigma"            # 3
)

mp@results$summary(vars)


##############################
#
#  Check bayesian models rs parameters are correct
#

offsets <- tibble(
   arm = c("Group-1", "Group-2"),
   offset = c(1, 3)
)

est_ind_slope <- posterior::summarise_draws(mp@results, "mean") |>
    filter(stringr::str_detect(variable, "^lm_rs_ind_rnd_slope")) |>
    rename(est = mean) |>
    select(est)

slopes <- dat_lm |>
    group_by(pt) |>
    slice(1) |>
    ungroup() |>
    select(pt, arm, actual = slope_ind) |>
    left_join(offsets, by = "arm") |>
    mutate(actual_offset = actual - offset) |>
    bind_cols(est_ind_slope) |>
    mutate(est_offset = est - offset)

mean(slopes$actual - slopes$est)
sd(slopes$actual - slopes$est)

ggplot(slopes, aes(x = est_offset, y = actual_offset)) +
    geom_point() +
    geom_abline()




##############################
#
# Test MLE OS parameters are correct from simulation
#

library(survival)
coxph(data = dat_os, Surv(time, event) ~ cov_cat + cov_cont)

mod <- survreg(Surv(time, event) ~ cov_cat + cov_cont, data = dat_os, dist = "weibull")

c(
    "lambda" = exp(-coef(mod)[["(Intercept)"]]/mod$scale),
    "gamma" = 1/mod$scale,
    -coef(mod)[-1]/mod$scale
)




##############################
#
# Test generated quantities work as expected
#


pts <- sample(dat_os$pt, 4)
samps <- LongitudinalQuantities(mp, groups = pts)
summary(samps)
as.data.frame(samps) |> tibble()
autoplot(samps)


