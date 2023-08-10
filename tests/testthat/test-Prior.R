test_that("Priors work as expected", {
    x <- prior_normal(4, 10)
    expect_equal(initialValues(x), 4)
    expect_equal(as.character(x), "normal(4, 10);")

    x <- prior_normal(4, 10, 20)
    expect_equal(initialValues(x), 20)
    expect_equal(as.character(x), "normal(4, 10);")
})


test_that("Invalid prior parameters are rejected", {
    expect_error(
        prior_normal(0, -1),
        regexp = "Invalid.*`sigma`"
    )

    expect_error(
        prior_beta(-3, 1),
        regexp = "Invalid.*`a`"
    )

    expect_error(
        prior_beta(5, -1),
        regexp = "Invalid.*`b`"
    )

    expect_error(
        prior_gamma(5, -1),
        regexp = "Invalid.*`beta`"
    )

    expect_error(
        prior_lognormal(5, -1),
        regexp = "Invalid.*`sigma`"
    )

    # Ensure that validation doesn't wrongly reject priors with no user specified parameters
    expect_s4_class(prior_none(), "Prior")
    expect_s4_class(prior_std_normal(), "Prior")
})
