
devtools::document()
devtools::load_all()
library(bayesplot)

jlist <- simulate_joint_data(
    n_arm = c(1000, 1000),
    times = 1:2000,
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.1,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_random_slope(phi = 0),
    os_fun = sim_os_weibull(
        lambda = 0.00333,
        gamma = 0.97
    )
)


dat_os <- jlist$os

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900)) |>
    dplyr::arrange(time, pt)


jm <- JointModel(
    survival = SurvivalWeibullPH()
)

write_stan(jm, "local/debug.stan")

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

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 1500,
    iter_warmup = 1000,
    chains = 1,
    exe_file = file.path("local", "full")
)

vars <- c(
    "sm_weibull_ph_lambda",   # 0.00333
    "sm_weibull_ph_gamma",    # 0.97
    "beta_os_cov"             # -0.1,  0.5,  0.3
)

mp$summary(vars)



mcmc_trace(mp$draws("sm_weibull_ph_lambda"))
mcmc_trace(mp$draws("sm_weibull_ph_gamma"))


library(survival)
coxph(
    Surv(time, event) ~ cov_cat + cov_cont,
    data = dat_os
)


