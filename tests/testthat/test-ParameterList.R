



test_that("ParameterList smoke tests", {
    pl <- ParameterList(
        Parameter(name = "inter", prior = prior_gamma(1, 2)),
        Parameter(name = "myp", prior = prior_normal(1, 4, init = 8))
    )

    # Can extract parameter names
    expect_equal(names(pl), c("inter", "myp"))

    # Can extract initial values
    actual <- initialValues(pl)
    expected <- list(
        "inter" = 1 / 2,
        "myp" = 8
    )
    expect_equal(actual, expected)

    # Can render to character
    actual <- c(
        "    inter ~ gamma(1, 2);",
        "    myp ~ normal(1, 4);"
    ) |>
        paste(collapse = "\n")

    expect_equal(actual, as.character(pl))
})
