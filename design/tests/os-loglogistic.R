
devtools::document()
devtools::load_all()
library(bayesplot)

options("jmpost.cache_dir" = file.path("local", "models"))

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
    os_fun = sim_os_loglogistic(
        lambda = 1/50,
        p = 1.1
    )
)


dat_os <- jlist$os

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% seq(0,900, by=75)) |>
    dplyr::arrange(time, pt)


jm <- JointModel(
    survival = SurvivalLogLogistic()
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
    iter_sampling = 1000,
    iter_warmup = 500,
    chains = 1
)

vars <- c(
    "sm_logl_lambda",   # 0.02
    "sm_logl_p",        # 1.1
    "beta_os_cov"       # -0.1,  0.5,  0.3
)

mp@results$summary(vars)



mcmc_trace(mp$draws("sm_logl_lambda"))
mcmc_trace(mp$draws("sm_logl_p"))


# TODO - Compre to survreg loglogistic
# library(survival)
# coxph(
#     Surv(time, event) ~ cov_cat + cov_cont,
#     data = dat_os
# )
