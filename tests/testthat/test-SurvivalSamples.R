mcmc_results <- get_mcmc_results()

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
        length(data_layer1$x),
        201L * 2L
    )
    expect_identical(
        length(unique(data_layer1$x)),
        201L
    )
    expect_identical(
        data_layer1$y,
        c(object[[1]]$summary$median, object[[2]]$summary$median)
    )

    data_layer2 <- layer_data(result, i = 2)
    expect_s3_class(data_layer2, "data.frame")
    expect_identical(
        names(data_layer2),
        c(
            "x", "ymin", "ymax", "PANEL", "group", "flipped_aes", "y",
            "colour", "fill", "linewidth", "linetype", "alpha"
        )
    )
    expect_identical(
        length(data_layer2$x),
        201L * 2L
    )
    expect_identical(
        length(unique(data_layer2$x)),
        201L
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
    # TODO - Need to rework when updating plotting functions
    ## vdiffr::expect_d  oppelganger("SurvivalSamples autoplot with KM", result)
})





test_that("summarise_by_group() works as expected", {
    get_ci_summary <- function(vec) {
        if (inherits(vec, "matrix")) {
            vec <- rowMeans(vec)
        }
        x <- data.frame(
            median = median(vec),
            lower = quantile(vec, 0.025),
            upper = quantile(vec, 0.975)
        )
        rownames(x) <- NULL
        x
    }

    x <- matrix(
        c(
            1, 10, 1, 3, 9,  23,
            2, 20, 1, 3, 10, 23,
            3, 30, 1, 3, 9,  23,
            4, 40, 2, 4, 12, 23,
            5, 50, 2, 4, 14, 23,
            6, 60, 2, 4, 10, 23
        ),
        byrow = TRUE,
        ncol = 6
    )
    colnames(x) <- sprintf(
        "quantity[%i,%i]",
    #     1  2  3  4  5  6    # Column index
        c(1, 1, 2, 2, 3, 3),
        c(4, 5, 4, 5, 4, 5)
    )
    draws_x <- posterior::as_draws_matrix(x)


    ## 1 subject 1 timepoint
    actual <- summarise_by_group(
        subject_index = 1,
        time_index = 4,
        quantities = draws_x
    )
    expect_equal(
        actual,
        get_ci_summary(x[, 1])
    )

    actual <- summarise_by_group(
        subject_index = 1,
        time_index = 5,
        quantities = draws_x
    )
    expect_equal(
        actual,
        get_ci_summary(x[, 2])
    )


    ## Select multiple subjects to collapse into a single "aggregate subject"
    ## at a single timepoint
    actual <- summarise_by_group(
        subject_index = c(1, 2),
        time_index = 5,
        quantities = draws_x
    )
    expect_equal(
        actual,
        get_ci_summary(rowMeans(x[, c(2, 4)]))
    )


    ## 1 subject at multiple time points
    actual <- summarise_by_group(
        subject_index = c(3),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual,
        dplyr::bind_rows(
            get_ci_summary(x[, 5]),
            get_ci_summary(x[, 6])
        )
    )


    ## Selecting multiple subjects to collapse into a single "agregate subject"
    ## at multiple timepoints
    actual <- summarise_by_group(
        subject_index = c(1, 3),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual,
        dplyr::bind_rows(
            get_ci_summary(x[, c(1, 5)]),
            get_ci_summary(x[, c(2, 6)])
        )
    )

    ## Can select the same subject multiple times
    actual <- summarise_by_group(
        subject_index = c(3, 3, 3, 2),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual,
        dplyr::bind_rows(
            get_ci_summary(x[, c(5, 5, 5, 3)]),
            get_ci_summary(x[, c(6, 6, 6, 4)])
        )
    )
})
