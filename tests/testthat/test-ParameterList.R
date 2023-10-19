



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
    expect_equal(
        as.StanModule(pl),
        StanModule(test_path("models", "ParameterList.stan"))
    )

    expected <- list(
        "prior_alpha_inter" = 1,
        "prior_beta_inter" = 2,
        "prior_mu_myp" = 1,
        "prior_sigma_myp" = 4
    )
    expect_equal(as_stan_list(pl), expected)
})


test_that("show() works for ParameterList objects", {

    x <- ParameterList(
        Parameter(name = "bob", prior = prior_normal(1, 4)),
        Parameter(name = "sam", prior = prior_beta(3, 1)),
        Parameter(name = "dave", prior = prior_lognormal(3, 2, init = 3), size = 4),
        Parameter(name = "steve", prior = prior_none())
    )

    expect_snapshot(print(x))
})
