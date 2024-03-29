library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

devtools::document()
devtools::load_all(export_all = FALSE)

options("jmpost.cache_dir" = file.path("local", "models"))

#### Example 1 - Fully specified model - using the defaults for everything
jm <- JointModel(
    longitudinal = LongitudinalRandomSlope(),
    survival = SurvivalExponential(),
    link = Link(
        link_dsld(),
        link_identity(
            prior = prior_normal(0, 0.02)
        )
    )
)



# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")


set.seed(514)
## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n = c(400, 300),
    times = 1:2000,
    lambda_cen = 1 / 400,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_random_slope(
        intercept = 30,
        sigma = 3,
        slope_mu = c(1,3),
        slope_sigma = 0.2,
        link_dsld = -0.1,
        link_identity = 0.0001
    ),
    os_fun = sim_os_exponential(
        lambda = 0.00333  # 1/300
    )
)


## Extract data to individual datasets
dat_os <- jlist$os
dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
    dplyr::arrange(time, pt)



## Prepare data for sampling
jdat <- DataJoint(
    subject = DataSubject(
        data = dat_os,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    survival = DataSurvival(
        data = dat_os,
        formula = Surv(time, event) ~ cov_cat + cov_cont
    ),
    longitudinal = DataLongitudinal(
        data = dat_lm,
        formula = sld ~ time,
        threshold = 5
    )
)

init_vals <- initialValues(jm, n_chains = 2)

## Sample from JointModel
mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 1000,
    iter_warmup = 1000,
    chains = 2,
    parallel_chains = 2,
    init = init_vals
)


### Select which parameters we actually care about
### Not all will exist depending on which model was run
vars <- c(
    "sm_exp_lambda",       # 0.00333
    "beta_os_cov",         # -0.1, 0.5, 0.3
    "lm_rs_intercept",     # 30
    "lm_rs_slope_mu",      # 1 , 3
    "lm_rs_slope_sigma",   # 0.2
    "lm_rs_sigma",         # 3
    "link_dsld",           # -0.1
    "link_identity"        # 0.0001
)

mp@results$summary(vars)




##############################
#
#  Check bayesian models rs parameters are correct
#

slopes <- tibble(
    est = mp$summary("lm_rs_rslope")$mean,
    actual = dat_lm |> group_by(pt) |> slice(1) |> pull(slope_ind)
)

mean(slopes$actual - slopes$est)
sd(slopes$actual - slopes$est)

ggplot(slopes, aes(x = est, y = actual)) +
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










