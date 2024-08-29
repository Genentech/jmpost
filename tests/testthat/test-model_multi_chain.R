
test_that("Can recover known distribution parameters from random slope model when using multiple chains", {
    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(
            intercept = prior_normal(30, 5),
            slope_sigma = prior_lognormal(log(0.2), sigma = 0.5),
            sigma = prior_lognormal(log(3), sigma = 0.5)
        ),
        survival = SurvivalExponential(
            lambda = prior_lognormal(log(1 / 200), 0.5)
        ),
        link = linkDSLD(prior = prior_normal(0.1, 0.2))
    )

    set.seed(3251)
    jlist <- SimJointData(
        design = list(
            SimGroup(150, "Arm-A", "Study-X"),
            SimGroup(150, "Arm-B", "Study-X")
        ),
        survival = SimSurvivalExponential(
            lambda = 1 / 200,
            time_max = 2000,
            lambda_censor = 1 / 9000,
            beta_cat = c(
                "A" = 0,
                "B" = -0.1,
                "C" = 0.5
            ),
            beta_cont = 0.3
        ),
        longitudinal = SimLongitudinalRandomSlope(
            times = c(1, 50, 100, 150, 200, 250, 300),
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2,
            link_dsld = 0.1
        ),
        .silent = TRUE
    )

    jdat <- DataJoint(
        subject = DataSubject(
            data = jlist@survival,
            subject = "subject",
            arm = "arm",
            study = "study"
        ),
        survival = DataSurvival(
            data = jlist@survival,
            formula = Surv(time, event) ~ cov_cat + cov_cont
        ),
        longitudinal = DataLongitudinal(
            data = jlist@longitudinal,
            formula = sld ~ time,
            threshold = 5
        )
    )

    mp <- run_quietly({
        sampleStanModel(
            jm,
            data = jdat,
            iter_sampling = 400,
            iter_warmup = 200,
            chains = 3,
            parallel_chains = 3
        )
    })


    vars <- c(
        "sm_exp_lambda" = 1 / 200,
        "beta_os_cov[1]" = -0.1,
        "beta_os_cov[2]" = 0.5,
        "beta_os_cov[3]" = 0.3,
        "lm_rs_intercept" = 30,
        "lm_rs_slope_mu[1]" = 1,
        "lm_rs_slope_mu[2]" = 3,
        "lm_rs_slope_sigma" = 0.2,
        "link_dsld" = 0.1
    )

    results_df <- cmdstanr::as.CmdStanMCMC(mp)$summary(names(vars))

    z_score <- abs(vars - results_df$mean) / results_df$sd
    expect_true(all(z_score < qnorm(0.95)))
})
