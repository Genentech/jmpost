
test_that("Can recover known distribution parameters from random slope model when using multiple chains", {
    jm <- JointModel(
        longitudinal = LongitudinalRandomSlope(),
        survival = SurvivalExponential(),
        link = LinkRandomSlope()
    )

    set.seed(3251)
    jlist <- simulate_joint_data(
        n = c(150, 150),
        times = 1:2000,
        lambda_cen = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3,
        lm_fun = sim_lm_random_slope(
            intercept = 30,
            sigma = 3,
            slope_mu = c(1, 3),
            slope_sigma = 0.2,
            phi = 0.1
        ),
        os_fun = sim_os_exponential(lambda = 1 / 200)
    )

    dat_os <- jlist$os
    dat_lm <- jlist$lm |>
        dplyr::filter(time %in% c(1, 50, 100, 150, 200, 250, 300)) |>
        dplyr::arrange(time, pt)

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

    mp <- sampleStanModel(
        jm,
        data = jdat,
        iter_sampling = 400,
        iter_warmup = 200,
        chains = 3,
        refresh = 0,
        parallel_chains = 3
    )

    vars <- c(
        "sm_exp_lambda" = 1 / 200,
        "beta_os_cov[1]" = -0.1,
        "beta_os_cov[2]" = 0.5,
        "beta_os_cov[3]" = 0.3,
        "lm_rs_intercept" = 30,
        "lm_rs_slope_mu[1]" = 1,
        "lm_rs_slope_mu[2]" = 3,
        "lm_rs_slope_sigma" = 0.2,
        "link_lm_phi" = 0.1
    )

    results_df <- mp@results$summary(names(vars))

    z_score <- abs(vars - results_df$mean) / results_df$sd
    expect_true(all(z_score < qnorm(0.95)))
})
