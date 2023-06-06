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
