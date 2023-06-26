# constructor ----

test_that("SurvivalSamples can be initialized", {
    x <- list(pt_00001 = 5, pt_00002 = 10)
    result <- .SurvivalSamples(x)
    expect_s4_class(result, "SurvivalSamples")
    expect_identical(names(result), names(x))
})

# subset ----

test_that("subsetting works as expected for SurvivalSamples", {
    object <- .SurvivalSamples(
        list(pt_00001 = 5, pt_00002 = 10)
    )
    result <- object["pt_00001"]
    expect_s4_class(result, "SurvivalSamples")
    expect_length(result, 1L)
    expect_identical(names(result), "pt_00001")
})

# aggregate ----

test_that("aggregate works as expected for SurvivalSamples", {
    x <- .SurvivalSamples(
        list(
            id1 = list(
                samples = matrix(1:4, 2, 2),
                summary = data.frame(time = 1:2, median = 0:1, lower = -1:0, upper = 1:2),
                observed = data.frame(t = 5, death = TRUE, median = 0.8, lower = 0.5, upper = 1.2)
            ),
            id2 = list(
                samples = matrix(2:5, 2, 2),
                summary = data.frame(time = 1:2, median = 0:1, lower = -1:0, upper = 1:2),
                observed = data.frame(t = 2, death = FALSE, median = 0.2, lower = 0.3, upper = 0.9)
            ),
            id3 = list(
                samples = matrix(3:6, 2, 2),
                summary = data.frame(time = 1:2, median = 0:1, lower = -1:0, upper = 1:2),
                observed = data.frame(t = 1, death = TRUE, median = 0.1, lower = -0.1, upper = 2)
            )
        )
    )
    result <- aggregate(x, groups = list(a = c("id3", "id1"), b = c("id1", "id2")))
    expect_s4_class(result, "SurvivalSamples")
    expect_identical(names(result), c("a", "b"))
    expect_identical(rownames(result[["a"]]$observed), c("id3", "id1"))
    expect_identical(rownames(result[["b"]]$observed), c("id1", "id2"))
})

# autoplot ----

test_that("autoplot works as expected for SurvivalSamples", {
    object <- survival(mcmc_results, patients = c("pt_00001", "pt_00022"))
    result <- expect_silent(autoplot(object))

    data_layer1 <- layer_data(result)
    expect_s3_class(data_layer1, "data.frame")
    expect_identical(
        names(data_layer1),
        c("x", "y", "PANEL", "group", "flipped_aes", "colour", "linewidth", "linetype", "alpha")
    )
    expect_identical(
        data_layer1$x,
        rep(mcmc_results@data$sm_time_grid, 2)
    )
    expect_identical(
        data_layer1$y,
        c(object[[1]]$summary$median, object[[2]]$summary$median)
    )

    data_layer2 <- layer_data(result, i = 2)
    expect_s3_class(data_layer2, "data.frame")
    expect_identical(
        names(data_layer2),
        c("x", "ymin", "ymax", "PANEL", "group", "flipped_aes", "y",
          "colour", "fill", "linewidth", "linetype", "alpha")
    )
    expect_identical(
        data_layer2$x,
        rep(mcmc_results@data$sm_time_grid, 2)
    )
    expect_identical(
        data_layer2$ymin,
        c(object[[1]]$summary$lower, object[[2]]$summary$lower)
    )
    expect_identical(
        data_layer2$ymax,
        c(object[[1]]$summary$upper, object[[2]]$summary$upper)
    )

    data_layer3 <- layer_data(result, i = 3)
    expect_s3_class(data_layer3, "data.frame")
    expect_identical(
        names(data_layer3),
        c("x", "y", "time", "survival", "status", "PANEL", "group", "colour",
          "fill", "linewidth", "linetype", "weight", "alpha")
    )
})

test_that("autoplot does not show the Kaplan-Meier plot if disabled", {
    object <- survival(mcmc_results, patients = c("pt_00001", "pt_00022"))
    result <- expect_silent(autoplot(object, add_km = FALSE))
    # Only 2 layers here, i.e. no Kaplan-Meier plot.
    expect_identical(length(result$layers), 2L)
})

test_that("autoplot works end to end with Kaplan-Meier plot", {
    object <- survival(mcmc_results, patients = c("pt_00001", "pt_00022"))
    result <- expect_silent(autoplot(object, add_km = TRUE))
    # 4 layers here, i.e. including Kaplan-Meier plot line and ticks.
    expect_identical(length(result$layers), 4L)
    vdiffr::expect_doppelganger("SurvivalSamples autoplot with KM", result)
})
