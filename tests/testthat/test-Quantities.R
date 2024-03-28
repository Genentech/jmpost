
test_that("Simple utility functions work as expected", {

    x <- Quantities(
        quantities = matrix(1:10, ncol = 2),
        groups = c("a", "b"),
        times = c(10, 20)
    )

    expect_equal(dim(x), c(5, 2))
    expect_equal(nrow(x), 5)
    expect_equal(ncol(x), 2)
})


test_that("collapse_quantities() works as expected", {
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


    collapser <- QuantityCollapser(
        times = 2,
        groups = "A",
        indexes = list(1)
    )
    actual <- collapse_quantities(x, collapser)
    expect_equal(
        actual |> samples_median_ci(),
        get_ci_summary(x[, 1])
    )

    ## Can average multiple columns together
    collapser <- QuantityCollapser(
        times = 99,
        groups = "A",
        indexes = list(c(1, 3))
    )
    actual <- collapse_quantities(x, collapser)
    expect_equal(
        actual |> samples_median_ci(),
        get_ci_summary(x[, c(1, 3)])
    )


    ## Can select the same subject in multiple groups
    collapser <- QuantityCollapser(
        times = c(13, 123),
        groups = c("A", "A"),
        indexes = list(c(1, 3), c(4, 1))
    )
    actual <- collapse_quantities(x, collapser)
    expect_equal(
        actual |> samples_median_ci(),
        dplyr::bind_rows(
            get_ci_summary(x[, c(1, 3)]),
            get_ci_summary(x[, c(4, 1)])
        )
    )


    ## Can select the same subject multiple times
    collapser <- QuantityCollapser(
        times = c(13, 123),
        groups = c("A", "A"),
        indexes = list(c(1, 3, 1, 1), c(4, 1))
    )
    actual <- collapse_quantities(x, collapser)
    expect_equal(
        actual |> samples_median_ci(),
        dplyr::bind_rows(
            get_ci_summary(x[, c(1, 3, 1, 1)]),
            get_ci_summary(x[, c(4, 1)])
        )
    )
})



test_that("summary.Quantiles works as expected", {
    raw_x <- matrix(1:10, ncol = 2)
    x <- Quantities(
        quantities = raw_x,
        groups = c("a", "a"),
        times = c(10, 20)
    )
    actual <- summary(x)
    expected <- data.frame(
        group = "a",
        time = c(10, 20),
        median = apply(raw_x, MARGIN = 2, median),
        lower = apply(raw_x, MARGIN = 2, quantile, 0.025),
        upper = apply(raw_x, MARGIN = 2, quantile, 0.975)
    )
    expect_equal(actual, expected)
})


test_that("as.data.frame.Quantiles works as expected", {
    raw_x <- matrix(1:10, ncol = 2)
    x <- Quantities(
        quantities = raw_x,
        groups = c("a", "b"),
        times = c(10, 20)
    )

    actual <- as.data.frame(x)
    expected <- data.frame(
        group = c(rep("a", 5), rep("b", 5)),
        time = c(rep(10, 5), rep(20, 5)),
        values = as.vector(raw_x)
    )
    expect_equal(actual, expected)
})


test_that("Quantities print method works as expected", {
    expect_snapshot({
        raw_x <- matrix(1:10, ncol = 2)
        x <- Quantities(
            quantities = raw_x,
            groups = c("a", "b"),
            times = c(10, 20)
        )
        print(x)
    })
})
