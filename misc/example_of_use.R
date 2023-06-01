library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

devtools::document()
devtools::load_all(export_all = FALSE)


#######################
#
#  Part 1 - Examples of specifying different models (not executed)
#
#



#### Example 1 - Fully specified model - using the defaults for everything

jm <- JointModel(
    longitudinal = LongitudinalRandomSlope(),
    link = LinkRandomSlope(),
    survival = SurvivalWeibullPH()
)




### Example 2 - Manually specify priors - Fit models independently (no link)

jm <- JointModel(
    link = LinkNone(),
    longitudinal = LongitudinalRandomSlope(
        intercept = prior_normal(40, 5),          # Just prior
        slope_mu = prior_normal(10, 2, init = 30) # Prior and init
    ),
    survival = SurvivalWeibullPH(
        lambda = prior_gamma(0.2, 0.5)
    )
)


### Example 3 - Specify survival model only

jm <- JointModel(
    survival = SurvivalWeibullPH()
)



### Example 4 - Specify longitudinal model only

jm <- JointModel(
    longitudinal = LongitudinalRandomSlope()
)




#######################
#
#
#  Part 2 - Example of fitting a model to data
#
#


jm <- JointModel(
    longitudinal = LongitudinalRandomSlope(),
    link = LinkRandomSlope(),
    survival = SurvivalWeibullPH()
)


# Create local file with stan code for debugging purposes ONLY
write_stan(jm, "local/debug.stan")


## Generate Test data with known parameters
jlist <- simulate_joint_data(
    n_arm = c(500, 500),
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
        phi = 0.1
    ),
    os_fun = sim_os_weibull(
        lambda = 0.00333,  # 1/300
        gamma = 0.97
    )
)


## Extract data to individual datasets, reduce longitudinal data to specific time points
dat_os <- jlist$os
dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
    dplyr::arrange(time, pt)



## Specify required variables to fit the model to within our dataset
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



## Sample from JointModel
mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 500,
    iter_warmup = 500,
    chains = 1,
    parallel_chains = 1,
    exe_file = file.path("local", "full")
)

## Inspect parameters from the model
vars <- c(
    "lm_rs_intercept",
    "lm_rs_slope_mu",
    "lm_rs_slope_sigma",
    "lm_rs_sigma",
    "link_lm_phi",
    "sm_weibull_ph_lambda",
    "sm_weibull_ph_gamma",
    "beta_os_cov"
)


mp$summary(vars)


str(mp)
mp$metadata()
