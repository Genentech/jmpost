
devtools::document()
devtools::load_all()
library(bayesplot)
library(survival)

jlist <- simulate_joint_data(
    n_arm = c(1000, 1000),
    times = seq(1, 1000, by = 0.5),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.3,
        "C" = 0.5
    ),
    beta_cont = 0.2,
    lm_fun = sim_lm_random_slope(phi = 0),
    os_fun = sim_os_exponential(lambda = 1/100)
)

dat_os <- jlist$os

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900)) |>
    dplyr::arrange(time, pt)


jm <- JointModel(
    survival = SurvivalExponential()
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
    parallel_chains = 1,
    thin = 1,
    exe_file = file.path("local", "full")
)

vars <- c(
    "sm_exp_lambda",    #  0.01
    "beta_os_cov[1]",   # -0.3
    "beta_os_cov[2]",   #  0.5
    "beta_os_cov[3]"    #  0.2
)

mp$summary(vars)



mcmc_trace(mp$draws("sm_exp_lambda"))







