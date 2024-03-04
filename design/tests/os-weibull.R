
devtools::document()
devtools::load_all()
library(bayesplot)

options("jmpost.cache_dir" = file.path("local", "models"))

set.seed(6042)

jlist <- simulate_joint_data(
    design = list(
        SimGroup(1000, "Arm-A", "Study-X"),
        SimGroup(1000, "Arm-B", "Study-X")
    ),
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
        lambda = 1/300,
        gamma = 0.97
    )
)


dat_os <- jlist$os

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900)) |>
    dplyr::arrange(time, pt)


jm <- JointModel(
    survival = SurvivalWeibullPH(
        lambda = prior_lognormal(log(1 / 300), 0.5),
        gamma = prior_lognormal(log(0.97), 0.5)
    )
)

write_stan(jm, "local/debug.stan")

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
    )
)

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 1500,
    iter_warmup = 1000,
    chains = 1
)

vars <- c(
    "sm_weibull_ph_lambda",   # 0.00333
    "sm_weibull_ph_gamma",    # 0.97
    "beta_os_cov"             # -0.1,  0.5,  0.3
)


mp@results$summary(vars)



mcmc_trace(mp$draws("sm_weibull_ph_lambda"))
mcmc_trace(mp$draws("sm_weibull_ph_gamma"))


library(survival)
coxph(
    Surv(time, event) ~ cov_cat + cov_cont,
    data = dat_os
)


