devtools::document()
devtools::load_all()




true_lambda <- 1/100

jlist <- simulate_joint_data(
    n_arm = c(500),
    times = seq(1, 1000, by = 0.5),
    lambda_cen = 1 / 9000,
    beta_cat = c( "A" = 0, "B" = 0, "C" = 0),
    beta_cont = 0,
    lm_fun = sim_lm_random_slope(phi = 0, slope_mu = 0),
    os_fun = sim_os_exponential(lambda = true_lambda)
)

dat_os <- jlist$os

dat_lm <- jlist$lm |>
    dplyr::filter(time %in% c(1, 100, 150, 200, 300, 400, 500, 600, 800, 900))

jm <- JointModel(survival = SurvivalExponential())

write_stan(jm, "local/debug.stan")

jdat <- DataJoint(
    survival = DataSurvival(
        data = dat_os,
        formula = Surv(time, event) ~ 1,
        subject = "pt",
        arm = "arm",
        study = "study",
        time_grid = 1
    ),
    longitudinal = DataLongitudinal(
        data = dat_lm,
        formula = sld ~ time,
        subject = "pt",
        time_grid = 1
    )
)

mp <- sampleStanModel(
    jm,
    data = jdat,
    iter_sampling = 200,
    iter_warmup = 200,
    chains = 1,
    parallel_chains = 1,
    exe_file = file.path("local", "full")
)

results_summary <- mp@results$summary("sm_exp_lambda")
lambda_mean <- results_summary$mean
lambda_sd <- results_summary$sd


z_score <- (lambda_mean - true_lambda) / lambda_sd
expect_true(abs(z_score) <= qnorm(0.99))



