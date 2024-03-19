





test_that("sim_os_loglogistic() is consistant with flexsurv", {
    t <- c(1, 4, 50, 200, 600)
    expect_equal(
        log(flexsurv::hllogis(t, scale = 400, shape = 2)),
        SimSurvivalLogLogistic(a = 400, b = 2)@loghazard(t)
    )
})



test_that("SurvivalLogLogistic can recover known values", {

    true_a <- 400
    true_b <- 2
    true_beta <- c(0.5, -0.2, 0.1)
    set.seed(837)

    jdat <- SimJointData(
        design = list(SimGroup(350, "Arm-A", "Study-X")),
        survival = SimSurvivalLogLogistic(
            a = true_a,
            b = true_b,
            lambda_censor = 1 / 9000,
            beta_cat = c("A" = 0, "B" = true_beta[1], "C" = true_beta[2]),
            beta_cont = true_beta[3],
        ),
        longitudinal = SimLongitudinalRandomSlope(slope_mu = 0),
        .silent = TRUE
    )

    dat_os <- jdat@survival

    jm <- JointModel(
        survival = SurvivalLogLogistic(
            a = prior_lognormal(log(400), 1),
            b = prior_lognormal(log(2), 1)
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
        )
    )

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_warmup = 300,
            iter_sampling = 400,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })

    # Variables to extract (order important)
    vars <- c("sm_loglogis_a", "sm_loglogis_b", "beta_os_cov")
    results_summary <- mp@results$summary(vars)

    # calculate Z-scores
    par_mean <- results_summary$mean
    par_sd <- results_summary$sd
    par_real <- c(true_a, true_b, true_beta)
    z_score <- (par_real - par_mean) / par_sd

    # Ensure Z-scores are within a reasonable margin of real values
    expect_true(all(abs(z_score) <= qnorm(0.99)))

})


test_that("Print method for SurvivalLogLogistic works as expected", {

    expect_snapshot({
        x <- SurvivalLogLogistic()
        print(x)
    })

    expect_snapshot({
        x <- SurvivalLogLogistic(
            beta = prior_gamma(3, 4),
            b = prior_cauchy(0, 1)
        )
        print(x)
    })
})
