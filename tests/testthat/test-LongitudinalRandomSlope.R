
test_that("Print method for LongitudinalRandomSlope works as expected", {

    expect_snapshot({
        x <- LongitudinalRandomSlope()
        print(x)
    })

    expect_snapshot({
        x <- LongitudinalRandomSlope(
            intercept = prior_normal(0, 1),
            sigma = prior_gamma(2, 1)
        )
        print(x)
    })
})

test_that("LongitudinalRandomSlope correctly generates an intercept per study", {
    set.seed(3521)
    sim_data <- simulate_joint_data(
        times = seq(1, 1000) / 365,
        design = list(
            SimGroup(80, "Arm-A", "Study-X"),
            SimGroup(120, "Arm-A", "Study-Y")
        ),
        lm_fun = sim_lm_random_slope(
            intercept = c(50, 70),
            slope_mu = 10,
            slope_sigma = 0.5,
            sigma = 2,
            link_dsld = 0,
            link_identity = 0
        ),
        os_fun = sim_os_exponential(lambda = 1 / 100),
        lambda_cen = 1 / 200,
        .silent = TRUE
    )

    dat_lm <- sim_data$lm |>
        dplyr::filter(time %in% (c(10, 100, 200, 500, 700, 900) / 365))

    jdat <- DataJoint(
        subject = DataSubject(
            data = sim_data$os,
            subject = "pt",
            arm = "arm",
            study = "study"
        ),
        longitudinal = DataLongitudinal(
            data = dat_lm,
            formula = sld ~ time
        )
    )

    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(60, 15),
            slope_mu = prior_normal(10, 5),
            slope_sigma = prior_lognormal(0.5, 1),
            sigma = prior_lognormal(2, 1)
        ),
        link = Link()
    )

    mp <- suppressWarnings(
        sampleStanModel(
            jm,
            data = jdat,
            iter_warmup = 200,
            iter_sampling = 400,
            chains = 2,
            refresh = 0,
            parallel_chains = 2
        )
    )

    samples <- mp@results$draws(
        c("lm_rs_intercept", "lm_rs_slope_mu", "lm_rs_slope_sigma", "lm_rs_sigma"),
        format = "draws_matrix"
    )

    ests <- apply(samples, 2, mean)
    ests_se <- sqrt(apply(samples, 2, var))
    z_score <- (ests - c(50, 70, 10, 0.5, 2)) / ests_se
    expect_true(all(abs(z_score) < qnorm(0.99)))
})
