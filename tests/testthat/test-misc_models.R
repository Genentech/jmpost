

test_that("Longitudinal Model doesn't print sampler rejection messages", {
    # These rejections typically happen when the sampler samples a
    # 0 value for the variance parameter. Sensible initial values +
    # setting near 0 limits (as opposed to 0) should avoid this
    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 2),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        )
    )
    set.seed(21932)
    ## Generate Test data with known parameters
    jlist <- simulate_joint_data(
        times = 1:1000,
        lambda_cen = 1 / 500,
        n = c(200, 200),
        lm_fun = sim_lm_random_slope(phi = 0),
        os_fun = sim_os_exponential(1 / 100)
    )


    ## Extract data to individual datasets
    dat_os <- jlist$os
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
        dplyr::arrange(time, pt)



    ## Prepare data for sampling
    jdat <- DataJoint(
        survival = DataSurvival(
            data = dat_os,
            formula = Surv(time, event) ~ cov_cat + cov_cont,
            subject = "pt",
            arm = "arm",
            study = "study",
            time_grid = c(1)
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            subject = "pt",
            threshold = 5,
            time_grid = c(1)
        )
    )

    mp <- capture_messages({
        devnull_out <- capture.output({
            devnull_model <- sampleStanModel(
                jm,
                data = jdat,
                iter_sampling = 3,
                iter_warmup = 3,
                chains = 1,
                refresh = 0,
                parallel_chains = 1,
                exe_dir = MODEL_DIR
            )
        })
    })

    expect_false(any(grepl("The current Metropolis proposal is about to be rejected", mp)))
})
