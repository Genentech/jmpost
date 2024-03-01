

test_that("smoke test for JointModelSamples", {
    set.seed(739)
    jlist <- simulate_joint_data(
        design = list(
            SimGroup(250, "Arm-A", "Study-X"),
            SimGroup(150, "Arm-B", "Study-X")
        ),
        times = 1:2000,
        lambda_cen = 1 / 9000,
        lm_fun = sim_lm_random_slope(
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2
        ),
        os_fun = sim_os_exponential(1 / 100),
        .debug = TRUE,
        .silent = TRUE
    )

    dat_os <- jlist$os
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% c(0, 1, 100, 200, 250, 300, 350)) |>
        dplyr::arrange(pt, time)


    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 2),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / 100), 1 / 100)
        )
    )

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
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time,
            threshold = 5
        )
    )

    expect_snapshot({
        devnull <- capture.output({
            suppressMessages({
                mpp <- sampleStanModel(
                    jm,
                    data = jdat,
                    iter_sampling = 100,
                    iter_warmup = 150,
                    chains = 1,
                    refresh = 0,
                    parallel_chains = 1
                )
            })
        })
        print(mpp)
    })

})
