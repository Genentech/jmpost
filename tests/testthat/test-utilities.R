# get_missing_rownumbers ----

test_that("get_missing_rownumbers works as expected", {
    df <- data.frame(
        time = c(1, 2, 3, NA),
        event = c(0, NA, 1, 1),
        x = c(NA, NA, NA, NA),
        y = c(NA, 5, 3, 7),
        z = c(1, 4, 3, NA)
    )

    actual <- get_missing_rownumbers(df, Surv(time, event) ~ y + z)
    expected <- c(1, 2, 4)
    expect_equal(actual, expected)
})

# remove_missing_rows ----

test_that("remove_missing_rows works as expected", {

    df <- data.frame(
        time = c(1, NA, 3, 5, 5, 4, 5, 5),
        event = c(0, 0, NA, 1, 0, 1, 1, 0),
        y = c(NA, 5, 3, NA, 3, 4, 3, 3),
        z = c(NA, 4, 3, 2, NA, 2, 4, 3),
        x = c(NA, NA, NA, NA, NA, NA, NA, NA),
        w = c("a", "b", "c", "d", "f", "h", "i", NA_character_)
    )

    expected <- data.frame(
        time =  c(4, 5),
        event = c(1, 1),
        y = c(4, 3),
        z = c(2, 4),
        x = c(NA, NA),
        w = c("h", "i")
    )
    suppressMessages({
        actual <- remove_missing_rows(df, Surv(time, event) ~ y, extra_vars = c("z", "w"))
    })

    rownames(expected) <- NULL
    rownames(actual) <- NULL

    expect_equal(expected, actual)
})

# expand_initial_values ----

test_that("expand_initial_values works as expected", {
    vals <- list("a" = 1, "b" = 2, "c" = c(1, 2), "d" = 10)
    siz <- list(
        "a" = structure(5, array = TRUE),
        "b" = structure(1, array = TRUE),
        "c" = structure(2, array = TRUE),
        "d" = structure(1, array = FALSE)
    )
    actual <- expand_initial_values(vals, siz)
    expected <- list(
        "a" = as.array(c(1, 1, 1, 1, 1)),
        "b" = as.array(2),
        "c" = as.array(c(1, 2)),
        "d" = 10
    )
    expect_equal(actual, expected)
})

# replace_with_lookup ----

test_that("replace_with_lookup works and sets array attributes as expected", {
    vals <- list(1, "b", "a", "d", 5)
    lku <- list("a" = 3, "b" = 2, "c" = 4, "d" = 1)
    actual <- replace_with_lookup(vals, lku)
    expected <- list(
        structure(1, array = FALSE),
        structure(2, array = TRUE),
        structure(3, array = TRUE),
        structure(1, array = TRUE),
        structure(5, array = TRUE)
    )
    expect_equal(actual, expected)
})

test_that("replace_with_lookup asserts sizes as numbers as expected", {
    vals <- list(1, "b", "a", 4, c(5, 6))
    lku <- list("a" = 3, "b" = 2, "c" = 4)
    expect_error(
        replace_with_lookup(vals, lku),
        regexp = "Existing values in sizes must be single numbers"
    )
})

test_that("replace_with_lookup asserts looked up sizes as numbers as expected", {
    vals <- list(1, "b", "a", 4, 5)
    lku <- list("a" = 3, "b" = c(2, 3), "c" = 1)
    expect_error(
        replace_with_lookup(vals, lku),
        regexp = "Selected values from data must be single numbers"
    )
})

# samples_median_ci ----

test_that("samples_median_ci works as expected", {
    samples <- matrix(
        data = c(
            -0.6, -0.2, 1.6, 0.1, 0.1, 1.7, 0.5, -1.3, -0.7, -0.4,
            0.9, 0.8, 0.4, 1.7, 3.1, 1, 1.2, 8.1, 1.7, 1.9,
            3, 3, 4, 4, 4, 3, 3, 3, 5, 4
        ),
        nrow = 10,
        ncol = 3,
        dimnames = list(NULL, c("par1", "par2", "par3"))
    )
    result <- expect_silent(samples_median_ci(samples))
    expected <- data.frame(
        median = c(-0.05, 1.45, 3.5),
        lower = c(-1.165, 0.49, 3),
        upper = c(1.6775, 6.975, 4.775),
        row.names = c("par1", "par2", "par3")
    )
    expect_equal(result, expected)
})

test_that("samples_median_ci works with a custom credibility level", {
    samples <- matrix(
        data = c(
            -0.6, -0.2, 1.6, 0.1, 0.1, 1.7, 0.5, -1.3, -0.7, -0.4,
            0.9, 0.8, 0.4, 1.7, 3.1, 1, 1.2, 8.1, 1.7, 1.9,
            3, 3, 4, 4, 4, 3, 3, 3, 5, 4
        ),
        nrow = 10,
        ncol = 3,
        dimnames = list(NULL, c("par1", "par2", "par3"))
    )
    result <- expect_silent(samples_median_ci(samples, level = 0.4))
    expected <- data.frame(
        median = c(-0.05, 1.45, 3.5),
        lower = c(-0.46, 0.97, 3),
        upper = c(0.22, 1.76, 4),
        row.names = c("par1", "par2", "par3")
    )

    expect_equal(result, expected)
})



test_that("validate_time_grid() works as expected", {

    expect_equal(
        validate_time_grid(c(1, 2, 3)),
        c(1, 2, 3)
    )
    expect_equal(
        validate_time_grid(c(1)),
        c(1)
    )
    expect_equal(
        validate_time_grid(c(1, 2, 30000.3)),
        c(1, 2, 30000.3)
    )
    expect_equal(
        validate_time_grid(c(1L, 2L, 4L)),
        c(1L, 2L, 4L)
    )


    ## Error handling
    expect_error(
        validate_time_grid(c(1, 1, 2)),
        regexp = "`time_grid`"
    )
    expect_error(
        validate_time_grid(c(2, 1, 3)),
        regexp = "`time_grid`"
    )
    expect_error(
        validate_time_grid(c(1, 3, NA)),
        regexp = "`time_grid`"
    )
    expect_error(
        validate_time_grid(c(1, 3, -Inf)),
        regexp = "`time_grid`"
    )
})


test_that("expand_patients() works as expected", {

    ## Smoke tests of basic usage
    expect_equal(
        expand_patients(c("A", "B"), c("A", "B", "C", "D")),
        c("A", "B")
    )
    expect_equal(
        expand_patients(c("B"), c("A", "B", "C", "D")),
        c("B")
    )
    expect_equal(
        expand_patients(NULL, c("A", "B", "C", "D")),
        c("A", "B", "C", "D")
    )
    expect_equal(
        expand_patients(NULL, c("A", "B", "C", "D", "D")),
        c("A", "B", "C", "D")
    )

    ## Error handling
    expect_error(
        expand_patients("E", c("A", "B", "C", "D")),
        regex = "`patients`"
    )
    expect_error(
        expand_patients(c("A", "A"), c("A", "B", "C", "D")),
        regex = "`patients`"
    )
    expect_error(
        expand_patients(c(1, 2), c("A", "B", "C", "D")),
        regex = "`patients`"
    )
})


test_that("decompose_patients() works as expected", {

    # Basic vector format
    actual <- decompose_patients(c("a", "b", "d"), c("a", "b", "c", "d"))
    expected <- list(
        groups = list(
            "a" = "a",
            "b" = "b",
            "d" = "d"
        ),
        unique_values = c("a", "b", "d"),
        indexes = list(
            "a" = 1,
            "b" = 2,
            "d" = 3
        )
    )
    expect_equal(actual, expected)



    # list format
    actual <- decompose_patients(
        list("g1" = c("b", "a"), "g2" = c("a", "d")),
        c("a", "b", "c", "d")
    )
    expected <- list(
        groups = list(
            "g1" = c("b", "a"),
            "g2" = c("a", "d")
        ),
        unique_values = c("a", "b", "d"),
        indexes = list(
            "g1" = c(2, 1),
            "g2" = c(1, 3)
        )
    )
    expect_equal(actual, expected)


    # NULL is correctly expanded
    actual <- decompose_patients(
        NULL,
        c("a", "d", "c", "b", "b", "b", "a")
    )
    expected <- list(
        groups = list(
            "a" = "a", "d" = "d", "c" = "c", "b" = "b"
        ),
        unique_values = c("a", "b", "c", "d"),
        indexes = list(
            "a" = 1, "d" = 4, "c" = 3, "b" = 2
        )
    )
    expect_equal(actual, expected)

    # errors if patient doesn't exist
    expect_error(
        decompose_patients("e", c("a", "d", "c", "b", "b")),
        regexp = "`patients`"
    )
    # errors if group has same patient twice
    expect_error(
        decompose_patients(list("g1" = c("a", "a")), c("a", "d", "c", "b", "b")),
        regexp = "`patients`"
    )

})


test_that("add_missing_stan_blocks() works as expected", {
    x <- list("data" = "abc", "model" = "das")
    actual <- add_missing_stan_blocks(x)
    expected <- STAN_BLOCKS
    for (i in names(expected)) expected[[i]] <- ""
    expected$data <- x$data
    expected$model <- x$model
    names_in_order <- c(
        "data", "model",
        names(expected)[!names(expected) %in% names(x)]
    )
    expect_equal(actual, expected[names_in_order])

    x <- list("data" = "abc", "model" = "das")
    actual <- add_missing_stan_blocks(x, list("el1" = 1, "model" = 2))
    expect_equal(actual, list("data" = "abc", "model" = "das", "el1" = ""))
})
