
test_data_1 <- ensure_test_data_1()

test_that("populationHR works as expected for default parameters", {
    mp <- test_data_1$jsamples
    set.seed(1231)
    result <- suppressWarnings(populationHR(object = mp))

    expect_matrix(
        result[[2]],
        any.missing = FALSE,
        ncols = 200,
        nrows = 13 # 10 baseline spline + 3 covariates
    )

    # Difference of beta_cat B and C from SimSurvivalExponential()
    expect_equal(
        0.2 - -0.4,
        result[[1]][["cov_catC", "mean"]] - result[[1]][["cov_catB", "mean"]],
        tolerance = 0.1
    )

    expect_equal(
        0.2,
        result[[1]][["cov_cont", "mean"]],
        tolerance = 0.1
    )

    # Summary calculations are match expectations
    summary_stats <- apply(result[[2]], 1, function(x) c(mean(x), quantile(x, c(0.5, 0.025, 0.975)))) |>
        t() |>
        as.data.frame()
    expect_equal(
        unname(summary_stats),
        unname(result[[1]])
    )
})

test_that("populationHR fails for bad input", {
    mp <- test_data_1$jsamples
    expect_error(populationHR(object = mp@data), "inherit from class")
    expect_error(populationHR(object = mp, hr_formula = "arm"), "formula")
    expect_error(populationHR(object = mp, baseline = ~arm), "time term")
    expect_error(populationHR(object = mp, hr_formula = ~XX), "All variables")
    expect_error(populationHR(object = mp, quantiles = c(-0.3, NA, 2)), "quantiles")
})


test_that("populationHR works as expected for alternative specfications", {
    mp <- test_data_1$jsamples
    set.seed(1231)
    # Arm + continuous covariate
    result_arm_cont <- populationHR(
        object = mp,
        baseline = ~splines::ns(time, df = 5),
        hr_formula = ~arm + cov_cont,
        quantiles = c(0.05, 0.95)
    )

    expect_matrix(
        result_arm_cont[[2]],
        any.missing = FALSE,
        ncols = 200,
        nrows = 7 # 5 baseline spline + 2 covariates
    )


    ### Summary calculations match expectations
    summary_stats <- apply(result_arm_cont[[2]], 1, function(x) c(mean(x), quantile(x, c(0.5, 0.05, 0.95)))) |>
        t() |>
        as.data.frame()

    expect_equal(
        unname(summary_stats),
        unname(result_arm_cont[[1]])
    )


    # Arm HR only
    set.seed(1231)
    result_arm <- populationHR(
        object = mp,
        baseline = ~splines::ns(time, df = 10),
        hr_formula = ~arm,
        quantiles = c(0.05, 0.95)
    )

    expect_matrix(
        result_arm[[2]],
        any.missing = FALSE,
        ncols = 200,
        nrows = 6 # 5 baseline spline + 1 covariates
    )

    # Summary calculations match expectations
    summary_stats <- apply(result_arm[[2]], 1, function(x) c(mean(x), quantile(x, c(0.5, 0.05, 0.95)))) |>
        t() |>
        as.data.frame()
    expect_equal(
        unname(summary_stats),
        unname(result[[1]])
    )
})
