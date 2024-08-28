
test_that("Print method for SurvivalGamma works as expected", {

    expect_snapshot({
        x <- SurvivalGamma()
        print(x)
    })

    expect_snapshot({
        x <- SurvivalGamma(
            k = prior_gamma(3, 4),
            theta = prior_cauchy(0, 1)
        )
        print(x)
    })
})



test_that("Can load and compile SurvivalGamma() model", {
    # Full joint model
    jm <- JointModel(
        longitudinal = LongitudinalGSF(centred = FALSE),
        survival = SurvivalGamma(),
        link = Link(linkShrinkage(), linkGrowth())
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)

    # Survival only submodel
    jm <- JointModel(
        survival = SurvivalGamma()
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})



test_that("SimSurvivalGamma() is consistant with flexsurv", {
    t <- c(1, 4, 50, 200, 600)
    expect_equal(
        log(flexsurv::hgamma(t, shape = 10, rate = 1 / 20)),
        SimSurvivalGamma(k = 10, theta = 20)@loghazard(t)
    )
})

test_that("SurvivalGamma can recover known values", {

    skip_if_not(is_full_test())

    true_k <- 10
    true_theta <- 20
    true_beta <- c(0.5, -0.2, 0.1)

    set.seed(1637)

    jdat <- SimJointData(
        design = list(SimGroup(300, "Arm-A", "Study-X")),
        survival = SimSurvivalGamma(
            k = true_k,
            theta = true_theta,
            lambda_censor = 1 / 9000,
            beta_cat = c("A" = 0, "B" = true_beta[1], "C" = true_beta[2]),
            beta_cont = true_beta[3],
        ),
        longitudinal = SimLongitudinalRandomSlope(slope_mu = 0),
        .silent = TRUE
    )

    dat_os <- jdat@survival

    jm <- JointModel(
        survival = SurvivalGamma(
            k = prior_lognormal(log(true_k), 1),
            theta = prior_lognormal(log(true_theta), 1)
        )
    )

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

    suppressWarnings({
        mp <- run_quietly({
            sampleStanModel(
                jm,
                data = jdat,
                iter_warmup = 300,
                iter_sampling = 400,
                chains = 2,
                refresh = 100,
                parallel_chains = 2
            )
        })
    })

    # Variables to extract (order important)
    vars <- c("sm_gamma_k", "sm_gamma_theta", "beta_os_cov")
    results_summary <- cmdstanr::as.CmdStanMCMC(mp)$summary(vars)

    # calculate Z-scores
    par_mean <- results_summary$mean
    par_sd <- results_summary$sd
    par_real <- c(true_k, true_theta, true_beta)
    z_score <- (par_real - par_mean) / par_sd

    # Ensure Z-scores are within a reasonable margin of real values
    expect_true(all(abs(z_score) <= qnorm(0.99)))
    expect_true(all(results_summary$ess_bulk > 100))

})
