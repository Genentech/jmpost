test_that("Priors work as expected", {

    x <- prior_normal(4, 10)
    expect_equal(initialValues(x), 4)
    expect_equal(
        as.StanModule(x, name = "bob"),
        StanModule(test_path("models", "Prior_1.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "bob"),
        list(prior_mu_bob = 4, prior_sigma_bob = 10)
    )

    x <- prior_normal(4, 10, 20)
    expect_equal(initialValues(x), 20)
    expect_equal(
        as.StanModule(x, name = "bob"),
        StanModule(test_path("models", "Prior_1.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "bob"),
        list(prior_mu_bob = 4, prior_sigma_bob = 10)
    )


    x <- prior_lognormal(log(4), 2)
    expect_equal(initialValues(x), exp(log(4) + 2))
    expect_equal(
        as.StanModule(x, name = "tim"),
        StanModule(test_path("models", "Prior_2.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "tim"),
        list(prior_mu_tim = log(4), prior_sigma_tim = 2)
    )

    x <- prior_lognormal(log(4), 2, 20)
    expect_equal(initialValues(x), 20)
    expect_equal(
        as.StanModule(x, name = "tim"),
        StanModule(test_path("models", "Prior_2.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "tim"),
        list(prior_mu_tim = log(4), prior_sigma_tim = 2)
    )

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



test_that("show() works for Prior objects", {
    expect_snapshot_output(print(prior_cauchy(0, 0.8, init = 4)))
    expect_snapshot_output(print(prior_normal(0, 0.8)))
    expect_snapshot_output(print(prior_std_normal()))
    expect_snapshot_output(print(prior_beta(5, 1)))
    expect_snapshot_output(print(prior_gamma(2.56, 12)))
    expect_snapshot_output(print(prior_none()))
})
