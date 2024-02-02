

test_that("SurvivalExponential can recover true parameter (no covariates)", {
    true_lambda <- 1 / 100

    set.seed(2043)
    jlist <- simulate_joint_data(
        n_arm = c(500),
        times = seq(1, 1000, by = 0.5),
        lambda_cen = 1 / 9000,
        beta_cat = c("A" = 0, "B" = 0, "C" = 0),
        beta_cont = 0,
        lm_fun = sim_lm_random_slope(phi = 0, slope_mu = 0),
        os_fun = sim_os_exponential(lambda = true_lambda)
    )

    dat_os <- jlist$os

    # Arbitrary data extraction until #192 is resolved
    dat_lm <- jlist$lm |> dplyr::filter(time %in% c(1, 100))

    jm <- JointModel(survival = SurvivalExponential())

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
        iter_sampling = 200,
        iter_warmup = 200,
        chains = 1,
        refresh = 0,
        parallel_chains = 1
    )

    results_summary <- mp@results$summary("sm_exp_lambda")
    lambda_mean <- results_summary$mean
    lambda_sd <- results_summary$sd

    z_score <- (lambda_mean - true_lambda) / lambda_sd
    expect_true(abs(z_score) <= qnorm(0.99))
})


test_that("SurvivalExponential can recover true parameter (including covariates)", {
    true_lambda <- 1 / 100
    true_beta <- c(0.5, -0.2, 0.1)
    set.seed(20234)
    jlist <- simulate_joint_data(
        n_arm = c(500),
        times = seq(1, 1000, by = 0.5),
        lambda_cen = 1 / 9000,
        beta_cat = c("A" = 0, "B" = true_beta[1], "C" = true_beta[2]),
        beta_cont = true_beta[3],
        lm_fun = sim_lm_random_slope(phi = 0, slope_mu = 0),
        os_fun = sim_os_exponential(lambda = true_lambda)
    )

    dat_os <- jlist$os

    # Arbitrary data extraction until #192 is resolved
    dat_lm <- jlist$lm |> dplyr::filter(time %in% c(1, 100))

    jm <- JointModel(survival = SurvivalExponential())

    jdat <-     jdat <- DataJoint(
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
        iter_sampling = 300,
        iter_warmup = 300,
        chains = 1,
        refresh = 0,
        parallel_chains = 1
    )

    # Variables to extract (order important)
    vars <- c("sm_exp_lambda", "beta_os_cov")
    results_summary <- mp@results$summary(vars)

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
