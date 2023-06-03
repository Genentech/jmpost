single_longitudinal <- JointModel(
    longitudinal = LongitudinalRandomSlope()
)
set.seed(321)
sim_data <- simulate_joint_data(
    lm_fun = sim_lm_random_slope(),
    os_fun = sim_os_exponential(1 / 300)
)
os_data <- sim_data$os
long_data <- sim_data$lm |>
    dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
    dplyr::arrange(time, pt)

joint_data <- DataJoint(
    survival = DataSurvival(
        data = os_data,
        formula = Surv(time, event) ~ cov_cat + cov_cont,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    longitudinal = DataLongitudinal(
        data = long_data,
        formula = sld ~ time,
        subject = "pt",
        threshold = 5
    )
)
mcmc_results <- sampleStanModel(
    single_longitudinal,
    data = joint_data,
    iter_sampling = 100,
    iter_warmup = 100,
    chains = 1,
    parallel_chains = 1,
    exe_file = tempfile()
)
# See https://mc-stan.org/cmdstanr/articles/cmdstanr-internals.html#saving-fitted-model-objects
# telling us to call draws() once to load all samples into the object in memory.
not_needed_now <- mcmc_results@results$draws()
saveRDS(mcmc_results, file = "mcmc_results.rds")
