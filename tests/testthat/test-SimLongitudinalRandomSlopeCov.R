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

test_that("old and covariate random-slope simulators are exactly consistent", {
    subjects <- data.frame(
        subject = paste0("S", 1:8),
        arm = factor(rep(c("A", "B"), each = 4)),
        study = factor(rep(c("X", "Y"), times = 4))
    )
    times <- c(0, 1, 3)
    old <- SimLongitudinalRandomSlope(
        times = times,
        intercept = c(30, 45),
        slope_mu = c(1, 2.5),
        slope_sigma = c(0.4, 0.7),
        sigma = 1.2
    )
    covariate <- SimLongitudinalRandomSlopeCov(
        times = times,
        mu_formula = ~study,
        slope_mu_formula = ~arm,
        slope_sigma_formula = ~arm,
        mu_intercept = 30,
        mu_coefficients = c(0, 15),
        slope_mu_intercept = 1,
        slope_mu_coefficients = c(0, 1.5),
        slope_sigma_intercept = log(0.4),
        slope_sigma_coefficients = c(0, log(0.7 / 0.4)),
        sigma = 1.2
    )

    set.seed(987)
    old_subjects <- sampleSubjects(old, subjects)
    set.seed(987)
    covariate_subjects <- sampleSubjects(covariate, subjects)
    expect_equal(covariate_subjects, old_subjects)

    observation_data <- do.call(
        rbind,
        lapply(times, function(time) {
            result <- old_subjects
            result$time <- time
            result
        })
    )
    set.seed(654)
    old_observations <- sampleObservations(old, observation_data)
    set.seed(654)
    covariate_observations <- sampleObservations(covariate, observation_data)
    expect_equal(covariate_observations, old_observations)
})

test_that("simulation coefficient helpers normalise supported inputs", {
    expect_equal(.simulation_coefficients(numeric(), 2, "beta"), c(0, 0))
    expect_equal(.simulation_coefficients(c(2, 3), 2, "beta"), c(2, 3))
    expect_equal(.simulation_coefficients(c(0, 2, 3), 2, "beta"), c(2, 3))
    expect_error(.simulation_coefficients(1:3, 2, "beta"), "starting with zero")
})
