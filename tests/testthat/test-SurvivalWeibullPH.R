
test_that("Print method for SurvivalWeibullPH works as expected", {

    expect_snapshot({
        x <- SurvivalWeibullPH()
        print(x)
    })


    expect_snapshot({
        x <- SurvivalWeibullPH(
            beta = prior_gamma(3, 4),
            gamma = prior_cauchy(0, 1)
        )
        print(x)
    })
})


test_that("SurvivalWeibullPH can recover known values", {

    true_lambda <- 1 / 300
    true_gamma <- 0.9
    true_beta <- c(0.5, -0.2, 0.1)
    set.seed(137)

    jdat <- SimJointData(
        design = list(SimGroup(300, "Arm-A", "Study-X")),
        survival = SimSurvivalWeibullPH(
            lambda = true_lambda,
            gamma = true_gamma,
            lambda_censor = 1 / 9000,
            beta_cat = c("A" = 0, "B" = true_beta[1], "C" = true_beta[2]),
            beta_cont = true_beta[3],
        ),
        longitudinal = SimLongitudinalRandomSlope(slope_mu = 0),
        .silent = TRUE
    )

    dat_os <- jdat@survival

    jm <- JointModel(
        survival = SurvivalWeibullPH(
            lambda = prior_lognormal(log(1 / 300), 1),
            gamma = prior_lognormal(log(1), 1)
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

    mp <- sampleStanModel(
        jm,
        data = jdat,
        iter_warmup = 300,
        iter_sampling = 400,
        chains = 1,
        refresh = 0,
        parallel_chains = 1
    )

    # Variables to extract (order important)
    vars <- c("sm_weibull_ph_lambda", "sm_weibull_ph_gamma", "beta_os_cov")
    results_summary <- mp@results$summary(vars)

    # calculate Z-scores
    par_mean <- results_summary$mean
    par_sd <- results_summary$sd
    par_real <- c(true_lambda, true_gamma, true_beta)
    z_score <- (par_real - par_mean) / par_sd

    # Ensure Z-scores are within a reasonable margin of real values
    expect_true(all(abs(z_score) <= qnorm(0.99)))

})
