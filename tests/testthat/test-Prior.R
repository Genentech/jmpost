test_that("Priors work as expected", {

    x <- prior_normal(4, 10)
    with_mocked_bindings(
        expect_equal(
            initialValues(x),
            4 * 0.5
        ),
        local_rnorm = \(...) 0
    )
    expect_equal(
        as.StanModule(x, name = "bob"),
        StanModule(test_path("models", "Prior_1.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "bob"),
        list(prior_mu_bob = 4, prior_sigma_bob = 10)
    )

    x <- prior_lognormal(log(4), 2)
    with_mocked_bindings(
        expect_equal(
            initialValues(x),
            exp(log(4) + 2) * 0.5
        ),
        local_rlnorm = \(...) 0
    )
    expect_equal(
        as.StanModule(x, name = "tim"),
        StanModule(test_path("models", "Prior_2.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "tim"),
        list(prior_mu_tim = log(4), prior_sigma_tim = 2)
    )


    tom <- prior_logistic(1, 2)
    dave <- prior_loglogistic(3, 4)
    jim <- prior_invgamma(5, 6)
    ben <- prior_student_t(7, 8, 9)
    kim <- prior_uniform(10, 11)

    header <- StanModule("parameters {
    real tom;
    real dave;
    real jim;
    real ben;
    real kim;
}")
    tom_sm <- as.StanModule(tom, name = "tom")
    dave_sm <- as.StanModule(dave, name = "dave")
    jim_sm <- as.StanModule(jim, name = "jim")
    ben_sm <- as.StanModule(ben, name = "ben")
    kim_sm <- as.StanModule(kim, name = "kim")

    full_sm <- list(header, tom_sm, dave_sm, jim_sm, ben_sm, kim_sm) %>%
        Reduce(merge, .)
    expect_equal(
        full_sm,
        StanModule(test_path("models", "Prior_3.stan"))
    )

    ## Check that the model syntax is correct (e.g. that we have
    ## correctly specified the stan prior distribution function names)
    model_obj <- cmdstanr::cmdstan_model(
        test_path("models", "Prior_3.stan"),
        compile = FALSE
    )
    expect_true(model_obj$check_syntax(quiet = TRUE))
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

    expect_error(
        prior_logistic(5, -1),
        regexp = "Invalid.*`sigma`"
    )

    expect_error(
        prior_loglogistic(5, -1),
        regexp = "Invalid.*`beta`"
    )

    expect_error(
        prior_loglogistic(-1, 6),
        regexp = "Invalid.*`alpha`"
    )

    expect_error(
        prior_student_t(-1, 6, 2),
        regexp = "Invalid.*`nu`"
    )
    expect_error(
        prior_student_t(1, 6, -2),
        regexp = "Invalid.*`sigma`"
    )

    expect_error(
        prior_invgamma(alpha = -1, beta = 2),
        regexp = "Invalid.*`alpha`"
    )
    expect_error(
        prior_invgamma(alpha = 1, beta = -2),
        regexp = "Invalid.*`beta`"
    )

    expect_error(
        prior_uniform(10, 9),
        regexp = "`alpha`` must be less than `beta`"
    )


    # Ensure that validation doesn't wrongly reject priors with no user specified parameters
    expect_s4_class(prior_init_only(prior_normal(3, 1)), "Prior")
    expect_s4_class(prior_std_normal(), "Prior")
})



test_that("show() works for Prior objects", {
    expect_snapshot(print(prior_cauchy(0, 0.8)))
    expect_snapshot(print(prior_normal(0, 0.8)))
    expect_snapshot(print(prior_std_normal()))
    expect_snapshot(print(prior_beta(5, 1)))
    expect_snapshot(print(prior_gamma(2.56, 12)))
    expect_snapshot(print(prior_init_only(prior_normal(1, 4))))
    expect_snapshot(print(prior_uniform(8, 10)))
    expect_snapshot(print(prior_student_t(3, 10, 4)))
    expect_snapshot(print(prior_logistic(sigma = 2, 10)))
    expect_snapshot(print(prior_loglogistic(1, 2)))
    expect_snapshot(print(prior_invgamma(alpha = 1, beta = 2)))
})


test_that("jmpost.prior_shrinkage works as expected", {
    x <- prior_normal(1, 2)
    with_mocked_bindings(
        {
            options("jmpost.prior_shrinkage" = 0.5)
            expect_equal(
                initialValues(x),
                1 * 0.5 + 4 * 0.5
            )

            options("jmpost.prior_shrinkage" = 0.9)
            expect_equal(
                initialValues(x),
                1 * 0.9 + 4 * 0.1
            )

            options("jmpost.prior_shrinkage" = 0.1)
            expect_equal(
                initialValues(x),
                1 * 0.1 + 4 * 0.9
            )

            ## Reset Shrinkage factor
            options("jmpost.prior_shrinkage" = 0.5)
        },
        local_rnorm = \(...) 4
    )
})



test_that("Limits work as expected", {
    x <- prior_normal(0, 1)
    x <- set_limits(x, lower = 0, upper = 1)
    ivs <- replicate(
        n = 100,
        initialValues(x)
    )
    expect_true(all(ivs > 0))
    expect_true(all(ivs < 1))

    expect_equal(
        as.StanModule(x, name = "bob")@model,
        "    bob ~ normal(prior_mu_bob, prior_sigma_bob) T[0, 1];"
    )


    x <- prior_cauchy(-200, 150)
    x <- set_limits(x, lower = 0)
    ivs <- replicate(
        n = 100,
        initialValues(x)
    )
    expect_true(all(ivs > 0))
    expect_equal(
        as.StanModule(x, name = "tim")@model,
        "    tim ~ cauchy(prior_mu_tim, prior_sigma_tim) T[0, ];"
    )


    ## Put an impossible constraint on the distribution
    x <- prior_lognormal(0, 1)
    x <- set_limits(x, upper = 0)
    expect_error(initialValues(x), regex = "Unable to generate")
    expect_equal(
        as.StanModule(x, name = "phil")@model,
        "    phil ~ lognormal(prior_mu_phil, prior_sigma_phil) T[, 0];"
    )
})



test_that("median(Prior) works as expected", {
    set.seed(2410)

    # Unrestricted
    p1 <- prior_normal(-200, 400)
    expect_equal(
        median(p1),
        -200,
        tolerance = 0.15
    )


    # Constrained
    p2 <- set_limits(p1, lower = 0)

    actual <- rnorm(6000, -200, 400) * 0.5 + -200 * 0.5
    actual_red <- actual[actual >= 0]

    expect_equal(
        median(p2),
        median(actual_red),
        tolerance = 0.15
    )
})


test_that("Parameters in priors must be length 1 #422", {
    expect_error(
        prior_normal(c(1, 2), 1),
        "Parameter `mu`"
    )

    expect_error(
        prior_normal(1, c(1, 2)),
        "Parameter `sigma`"
    )

    expect_error(
        prior_normal(c(1, 2), c(1, 2)),
        "Parameter `mu`"
    )

    expect_error(
        prior_gamma(c(1, 2), 2),
        "Parameter `alpha`"
    )
})
