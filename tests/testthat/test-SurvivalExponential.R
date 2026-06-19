test_that("Can load and compile SurvivalExponential() model", {
    # Full joint model
    jm <- JointModel(
        longitudinal = LongitudinalGSF(centred = FALSE),
        survival = SurvivalExponential(),
        link = Link(linkShrinkage(), linkGrowth())
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)

    # Survival only submodel
    jm <- JointModel(
        survival = SurvivalExponential()
    )
    x <- as.StanModule(jm)
    expect_stan_syntax(x)
})


test_that("SurvivalExponential can recover true parameter (including covariates)", {
    skip_if_not(is_full_test())

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
        longitudinal = SimLongitudinalRandomSlope(
            slope_mu = 0,
            slope_sigma = 0.5
        )
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
            parallel_chains = 1,
            seed = 123
        )
    })

    # Variables to extract (order important)
    vars <- c("sm_exp_lambda", "beta_os_cov")
    results_summary <- cmdstanr::as.CmdStanMCMC(mp)$summary(vars)

    # calculate Z-scores
    par_mean <- results_summary$mean
    par_sd <- results_summary$sd
    par_real <- c(true_lambda, true_beta)
    z_score <- (par_real - par_mean) / par_sd

    # Ensure Z-scores are within a reasonable margin of real values
    expect_true(all(abs(z_score) <= qnorm(0.99)))
    expect_true(all(results_summary$ess_bulk > 50))
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

test_that("Different priors for the beta components are possible", {
    # Same iid prior for all beta components:
    expect_snapshot({
        x <- SurvivalExponential(beta = prior_normal(0, 1))
        print(x)
    })

    # Different priors for each beta component:
    expect_snapshot({
        x <- SurvivalExponential(
            beta = prior_normal_vector(c(0, 1, 2), c(1, 2, 3))
        )
        print(x)
    })

    skip_if_not(is_full_test())

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
        longitudinal = SimLongitudinalRandomSlope(
            slope_mu = 0,
            slope_sigma = 0.5
        )
    )

    dat_os <- jdat@survival

    jm <- JointModel(
        survival = SurvivalExponential(
            beta = prior_normal_vector(c(0, 1, 2), c(1, 2, 3))
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

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 600,
            iter_warmup = 500,
            chains = 1,
            refresh = 0,
            parallel_chains = 1,
            seed = 123
        )
    })

    # Variables to extract (order important)
    vars <- c("sm_exp_lambda", "beta_os_cov")
    results_summary <- cmdstanr::as.CmdStanMCMC(mp)$summary(vars)
})
