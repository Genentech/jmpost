
get_mcmc_results <- function() {
    joint_model <- JointModel(
        longitudinal = LongitudinalRandomSlope(),
        survival = SurvivalExponential(),
        link = LinkRandomSlope()
    )
    set.seed(321)
    sim_data <- simulate_joint_data(
        lm_fun = sim_lm_random_slope(),
        os_fun = sim_os_exponential(1 / 30),
        lambda_cen = 1 / 100
    )
    os_data <- sim_data$os
    long_data <- sim_data$lm |>
    dplyr::filter(time %in% c(0, 10, 50, 100, 150, 200, 250, 300)) |>
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
    # Cached model takes a second to run
    mcmc_results <- sampleStanModel(
        joint_model,
        data = joint_data,
        iter_sampling = 100,
        iter_warmup = 100,
        chains = 1,
        refresh = 0,
        parallel_chains = 1
    )
    # See https://mc-stan.org/cmdstanr/articles/cmdstanr-internals.html#saving-fitted-model-objects
    # telling us to call draws() once to load all samples into the object in memory.
    not_needed_now <- mcmc_results@results$draws()
    return(mcmc_results)
}
