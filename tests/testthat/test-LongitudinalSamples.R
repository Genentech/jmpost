# constructor ----

test_that("LongitudinalSamples can be initialized", {
    x <- list(pt_00001 = 5, pt_00002 = 10)
    result <- .LongitudinalSamples(x)
    expect_s4_class(result, "LongitudinalSamples")
    expect_identical(names(result), names(x))
})

# subset ----

test_that("subsetting works as expected for LongitudinalSamples", {
    object <- .LongitudinalSamples(
        list(pt_00001 = 5, pt_00002 = 10)
    )
    result <- object["pt_00001"]
    expect_s4_class(result, "LongitudinalSamples")
    expect_length(result, 1L)
    expect_identical(names(result), "pt_00001")
})

# autoplot ----

test_that("autoplot works as expected for LongitudinalSamples", {
    mcmc_results <- readRDS(test_path("fixtures", "mcmc_results.rds"))
    object <- longitudinal(mcmc_results, patients = c("pt_00001", "pt_00022"))
    result <- expect_silent(autoplot(object))

    data_layer1 <- layer_data(result)
    expect_s3_class(data_layer1, "data.frame")
    expect_identical(
        names(data_layer1),
        c("x", "y", "PANEL", "group", "flipped_aes", "colour", "linewidth", "linetype", "alpha")
    )
    expect_identical(
        data_layer1$x,
        rep(mcmc_results@data$lm_time_grid, 2)
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
        rep(mcmc_results@data$lm_time_grid, 2)
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
        c("x", "y", "PANEL", "group", "shape", "colour", "size", "fill",
          "alpha", "stroke")
    )
})
