

devtools::document()
devtools::load_all()
library(bayesplot)

jlist <- simulate_joint_data(
    n_arm = c(1000, 1000),
    times = seq(1, 1000, by = 0.5),
    lambda_cen = 1 / 9000,
    beta_cat = c(
        "A" = 0,
        "B" = -0.3,
        "C" = 0.5
    ),
    beta_cont = 0.3,
    lm_fun = sim_lm_random_slope(phi = 0),
    os_fun = sim_os_exponential(lambda = log(1/100))
)

dat_os <- jlist$os

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900)) |>
    dplyr::arrange(time, pt)


jm <- JointModel(
    survival_model = SurvivalExponential()
)

write_stan(jm, "local/debug.stan")

stan_data <- as_stan_data(dat_os, dat_lm, ~ cov_cat + cov_cont)
mp <- sampleStanModel(
    jm,
    data = stan_data,
    iter_sampling = 1500,
    iter_warmup = 1000,
    chains = 1,
    parallel_chains = 1,
    thin = 1,
    exe_file = file.path("local", "full")
)

vars <- c(
    "sm_exp_lambda",
    "beta_os_cov[1]",
    "beta_os_cov[2]",
    "beta_os_cov[3]"
)

mp$summary(vars)



mcmc_trace(mp$draws("sm_weibull_ph_lambda"))
mcmc_trace(mp$draws("sm_weibull_ph_gamma"))


library(survival)
coxph(
    Surv(time, event) ~ cov_cat + cov_cont,
    data = dat_os
)










