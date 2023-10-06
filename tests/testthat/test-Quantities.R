
test_that("Simple utility functions work as expected", {

    x <- Quantities(list(
        matrix(1:10, ncol = 2),
        matrix(1:10, ncol = 2),
        matrix(1:10, ncol = 2),
        matrix(1:10, ncol = 2)
    ))

    expect_equal(length(x), 4)
    expect_equal(dim(x), c(5, 2))
    expect_equal(nrow(x), 5)
    expect_equal(ncol(x), 2)
})


test_that("average_samples_by_index() works as expected", {
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
        # 1  2  3  4  5  6    # Column index
        c(1, 1, 2, 2, 3, 3),  # Subject IDs
        c(4, 5, 4, 5, 4, 5)   # Time point IDs
    )
    draws_x <- posterior::as_draws_matrix(x)


    ## 1 subject 1 timepoint
    actual <- average_samples_by_index(
        subject_index = 1,
        time_index = 4,
        quantities = draws_x
    )
    expect_equal(
        actual |> samples_median_ci(),
        get_ci_summary(x[, 1])
    )

    actual <- average_samples_by_index(
        subject_index = 1,
        time_index = 5,
        quantities = draws_x
    )
    expect_equal(
        actual |> samples_median_ci(),
        get_ci_summary(x[, 2])
    )


    ## Select multiple subjects to collapse into a single "aggregate subject"
    ## at a single timepoint
    actual <- average_samples_by_index(
        subject_index = c(1, 2),
        time_index = 5,
        quantities = draws_x
    )
    expect_equal(
        actual |> samples_median_ci(),
        get_ci_summary(rowMeans(x[, c(2, 4)]))
    )


    ## 1 subject at multiple time points
    actual <- average_samples_by_index(
        subject_index = c(3),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual |> samples_median_ci(),
        dplyr::bind_rows(
            get_ci_summary(x[, 5]),
            get_ci_summary(x[, 6])
        )
    )


    ## Selecting multiple subjects to collapse into a single "agregate subject"
    ## at multiple timepoints
    actual <- average_samples_by_index(
        subject_index = c(1, 3),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual |> samples_median_ci(),
        dplyr::bind_rows(
            get_ci_summary(x[, c(1, 5)]),
            get_ci_summary(x[, c(2, 6)])
        )
    )

    ## Can select the same subject multiple times
    actual <- average_samples_by_index(
        subject_index = c(3, 3, 3, 2),
        time_index = c(4, 5),
        quantities = draws_x
    )
    expect_equal(
        actual |> samples_median_ci(),
        dplyr::bind_rows(
            get_ci_summary(x[, c(5, 5, 5, 3)]),
            get_ci_summary(x[, c(6, 6, 6, 4)])
        )
    )
})



test_that("summary.Quantiles works as expected", {
    raw_x <- matrix(1:10, ncol = 2)
    x <- Quantities(list(raw_x))
    actual <- summary(x, c(10, 20), type = "i", groups = list("a" = 1))
    expected <- data.frame(
        median = apply(raw_x, MARGIN = 2, median),
        lower = apply(raw_x, MARGIN = 2, quantile, 0.025),
        upper = apply(raw_x, MARGIN = 2, quantile, 0.975),
        time = c(10, 20),
        group = "a",
        type = "i"
    )
    expect_equal(actual, expected)
})


test_that("as.data.frame.Quantiles works as expected", {
    raw_x1 <- matrix(1:10, ncol = 2)
    raw_x2 <- matrix(21:30, ncol = 2)
    x <- Quantities(list(raw_x1, raw_x2))

    actual <- as.data.frame(x, time_grid = c(10, 20), type = "i", groups = list("a" = 1, "b" = 2))
    expected <- data.frame(
        values = c(as.vector(raw_x1), as.vector(raw_x2)),
        time = c(rep(10, 5), rep(20, 5), rep(10, 5), rep(20, 5)),
        group = c(rep("a", 10), rep("b", 10)),
        type = "i"
    )
    expect_equal(actual, expected)
})
