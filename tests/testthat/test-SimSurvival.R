
test_that("SimSurvivalExponential creates a dataset with the correct parameter", {
    set.seed(321)
    sim_data <- SimJointData(
        design = list(
            SimGroup(200, "Arm-A", "Study-X"),
            SimGroup(200, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalRandomSlope(times = c(0)),
        survival = SimSurvivalExponential(
            time_max = 2000,
            time_step = 1,
            lambda_censor = 1 / 5000,
            lambda = 1 / 100,
            beta_cont = 1.2,
            beta_cat = c("A" = 0, "B" = -0.6, "C" = 0.9)
        ),
        .silent = TRUE
    )

    mod <- survival::survreg(
        survival::Surv(time, event) ~ cov_cont + cov_cat,
        data = sim_data@survival,
        dist = "exponential"
    )

    par_real <- c(-log(1 / 100), 1.2, -0.6, 0.9)
    par_obs <- c(coef(mod)[1], -coef(mod)[-1])
    par_obs_se <- sqrt(diag(vcov(mod)))
    z_score <- (par_obs - par_real) / par_obs_se
    expect_true(all(abs(z_score) < qnorm(0.99)))
})


test_that("SimSurvivalWeibullPH creates a dataset with the correct parameter", {
    set.seed(1310)
    lambda_real <- 1 / 100
    gamma_real <- 0.9
    sim_data <- SimJointData(
        design = list(
            SimGroup(400, "Arm-A", "Study-X"),
            SimGroup(400, "Arm-B", "Study-X")
        ),
        longitudinal = SimLongitudinalRandomSlope(times = c(0)),
        survival = SimSurvivalWeibullPH(
            time_max = 2000,
            time_step = 1,
            lambda_censor = 1 / 5000,
            lambda = lambda_real,
            gamma = gamma_real,
            beta_cont = 1.2,
            beta_cat = c("A" = 0, "B" = -0.6, "C" = 0.9)
        ),
        .silent = TRUE
    )
    mod <- survival::survreg(
        survival::Surv(time, event) ~ cov_cont + cov_cat,
        data = sim_data@survival,
        dist = "weibull"
    )
    scale <- mod$scale

    ## Check covariate coefficients
    alpha_real <- -c(1.2, -0.6, 0.9) * scale  # Convert from PH to scale-location formulation
    alpha_obs <- coef(mod)[-1]
    alpha_se <- sqrt(diag(vcov(mod)[2:4, 2:4]))
    z_score <- (alpha_obs - alpha_real) / alpha_se
    expect_true(all(abs(z_score) < qnorm(0.9)))

    ## Check Lambda parameter
    int_real <- -log(lambda_real) * (scale)
    int_obs <- coef(mod)[1]
    int_obs_se <- sqrt(vcov(mod)[1, 1])
    z_score <- (int_real - int_obs) / int_obs_se
    expect_true(abs(z_score) < qnorm(0.9))

    ## Check Scale Parameter
    log_scale_real <- log(1 / gamma_real)
    log_scale_obs <- log(scale)
    log_scale_se <- sqrt(vcov(mod)["Log(scale)", "Log(scale)"])
    z_score <- (log_scale_obs - log_scale_real) / log_scale_se
    expect_true(abs(z_score) < qnorm(0.9))
})

test_that("print methods work as expected", {
    expect_snapshot(print(SimSurvival(loghazard = \(x) x)))
    expect_snapshot(print(SimSurvivalExponential(lambda = 5)))
    expect_snapshot(print(SimSurvivalLogLogistic(a = 5, b = 0.5)))
    expect_snapshot(print(SimSurvivalWeibullPH(lambda = 5, gamma = 5)))
})
