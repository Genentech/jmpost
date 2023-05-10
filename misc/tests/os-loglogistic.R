devtools::document()
devtools::load_all()
library(bayesplot)
library(survival)

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
  os_fun = sim_os_loglogistic(
    lambda = 1 / 50,
    p = 1.1
  )
)


dat_os <- jlist$os

dat_lm <- jlist$lm |>
  dplyr::filter(time %in% c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900)) |>
  dplyr::arrange(time, pt)


jm <- JointModel(
  survival = SurvivalLogLogistic()
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
  iter_sampling = 1000,
  iter_warmup = 500,
  chains = 1,
  exe_file = file.path("local", "full")
)

vars <- c(
  "sm_logl_lambda", # 0.02
  "sm_logl_p", # 1.1
  "beta_os_cov" # -0.1,  0.5,  0.3
)

mp$summary(vars)



mcmc_trace(mp$draws("sm_logl_lambda"))
mcmc_trace(mp$draws("sm_logl_p"))


# TODO - Compre to survreg loglogistic
# library(survival)
# coxph(
#     Surv(time, event) ~ cov_cat + cov_cont,
#     data = dat_os
# )
