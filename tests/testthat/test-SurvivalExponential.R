
test_that("SurvivalExponential can recover true parameter (including covariates)", {
    true_lambda <- 1 / 100
    true_beta <- c(0.5, -0.2, 0.1)
    set.seed(2034)
    jdat <- SimJointData(
        design = list(SimGroup(700, "Arm-A", "Study-X")),
        survival = SimSurvivalExponential(
            lambda = true_lambda,
            lambda_censor = 1 / 9000,
            beta_cat = c("A" = 0, "B" = true_beta[1], "C" = true_beta[2]),
            beta_cont = true_beta[3],
        ),
        longitudinal = SimLongitudinalRandomSlope(slope_mu = 0)
    )

    dat_os <- jdat@survival

    jm <- JointModel(survival = SurvivalExponential())

    jdat <- DataJoint(
        subject = DataSubject(
            data = dat_os,
            subject = "subject",
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
            iter_sampling = 600,
            iter_warmup = 500,
            chains = 1,
            refresh = 0,
            parallel_chains = 1
        )
    })

    # Variables to extract (order important)
    vars <- c("sm_exp_lambda", "beta_os_cov")
    results_summary <- as.CmdStanMCMC(mp)$summary(vars)

    # calculate Z-scores
    par_mean <- results_summary$mean
    par_sd <- results_summary$sd
    par_real <- c(true_lambda, true_beta)
    z_score <- (par_real - par_mean) / par_sd

    # Ensure Z-scores are within a reasonable margin of real values
    expect_true(all(abs(z_score) <= qnorm(0.99)))
})


test_that("Print method for SurvivalExponential works as expected", {
    expect_snapshot({
        x <- SurvivalExponential()
        print(x)
    })

    expect_snapshot({
        x <- SurvivalExponential(beta = prior_gamma(3, 4))
        print(x)
    })
})
