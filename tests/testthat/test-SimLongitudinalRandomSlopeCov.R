test_that("SimLongitudinalRandomSlopeCov uses its covariate predictors", {
    set.seed(123)
    sim <- SimLongitudinalRandomSlopeCov(
        times = c(0, 1),
        mu_formula = ~study,
        slope_mu_formula = ~arm,
        slope_sigma_formula = ~arm,
        mu_intercept = 30,
        mu_coefficients = c(0, 10),
        slope_mu_intercept = 1,
        slope_mu_coefficients = c(0, 2),
        slope_sigma_intercept = log(0.5),
        slope_sigma_coefficients = c(0, log(2)),
        sigma = 0.1
    )
    subjects <- data.frame(
        subject = paste0("S", 1:4),
        arm = factor(c("A", "B", "A", "B")),
        study = factor(c("X", "X", "Y", "Y"))
    )

    set.seed(456)
    sampled <- sampleSubjects(sim, subjects)

    expect_true(is(sim, "SimLongitudinalRandomSlopeCov"))
    expect_equal(sampled$intercept, c(30, 30, 40, 40))
    set.seed(456)
    expect_equal(
        sampled$slope_ind,
        c(
            rnorm(1, 1, 0.5),
            rnorm(1, 3, 1),
            rnorm(1, 1, 0.5),
            rnorm(1, 3, 1)
        )
    )
})

test_that("SimLongitudinalRandomSlopeCov works in SimJointData", {
    set.seed(321)
    result <- SimJointData(
        design = list(
            SimGroup(3, "A", "X"),
            SimGroup(3, "B", "X")
        ),
        longitudinal = SimLongitudinalRandomSlopeCov(times = c(0, 1)),
        survival = SimSurvivalExponential(
            lambda = 0.01,
            time_max = 2,
            time_step = 1
        ),
        .silent = TRUE
    )

    expect_true(is(result, "SimJointData"))
    expect_equal(length(unique(result@longitudinal$subject)), 6)
})
