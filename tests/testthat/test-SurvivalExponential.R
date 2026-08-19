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

test_that("SurvivalExponential() can fix lambda with prior_const()", {
    default_model <- JointModel(survival = SurvivalExponential())
    default_stan_code <- paste(
        as.character(as.StanModule(default_model)),
        collapse = "\n"
    )
    expect_match(
        default_stan_code,
        "real<lower=[^>]+> sm_exp_lambda;"
    )

    jm <- JointModel(
        survival = SurvivalExponential(lambda = prior_const(1))
    )

    x <- as.StanModule(jm)
    expect_stan_syntax(x)

    stan_code <- paste(as.character(x), collapse = "\n")
    expect_match(stan_code, "real prior_const_sm_exp_lambda;", fixed = TRUE)
    expect_match(
        stan_code,
        "real<lower=[^>]+> sm_exp_lambda = prior_const_sm_exp_lambda;"
    )
    expect_false(grepl(
        "real<lower=[^>]+> sm_exp_lambda;",
        stan_code,
        perl = TRUE
    ))

    expect_equal(
        as_stan_list(jm@parameters)$prior_const_sm_exp_lambda,
        1
    )
    expect_false(
        "sm_exp_lambda" %in% names(initialValues(jm, n_chains = 1)[[1]])
    )
})

test_that("SurvivalExponential() can fix beta with prior_const_vector()", {
    beta_const <- JointModel(
        survival = SurvivalExponential(beta = prior_const(0))
    )
    expect_stan_syntax(as.StanModule(beta_const))

    beta_const_vector <- JointModel(
        survival = SurvivalExponential(
            beta = prior_const_vector(c(1, 2, 3))
        )
    )
    beta_const_vector_module <- as.StanModule(beta_const_vector)
    expect_stan_syntax(beta_const_vector_module)
    beta_const_vector_code <- paste(
        as.character(beta_const_vector_module),
        collapse = "\n"
    )
    expect_match(
        beta_const_vector_code,
        "vector[p_os_cov_design] prior_const_beta_os_cov;",
        fixed = TRUE
    )
    expect_match(
        beta_const_vector_code,
        paste0(
            "vector[p_os_cov_design] beta_os_cov = ",
            "prior_const_beta_os_cov;"
        ),
        fixed = TRUE
    )
    expect_equal(
        as_stan_list(beta_const_vector@parameters)$prior_const_beta_os_cov,
        c(1, 2, 3)
    )
})

test_that("SurvivalExponential() does not print truncation for prior_const()", {
    x <- SurvivalExponential(lambda = prior_const(1))
    expect_equal(
        as.character(x@parameters@parameters[[1]]),
        "sm_exp_lambda = const(value = 1)"
    )
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
    x <- SurvivalExponential()
    expect_snapshot(print(x))

    x <- SurvivalExponential(beta = prior_gamma(3, 4))
    expect_snapshot(print(x))

    x <- SurvivalExponential(lambda = prior_const(1))
    expect_snapshot(print(x))
})

test_that("Different priors for the beta components are possible", {
    # Same iid prior for all beta components:
    x <- SurvivalExponential(beta = prior_normal(0, 1))
    expect_snapshot(print(x))

    # Different priors for each beta component:
    x <- SurvivalExponential(
        beta = prior_normal_vector(c(0, 1, 2), c(1, 2, 3))
    )
    expect_snapshot(print(x))

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
