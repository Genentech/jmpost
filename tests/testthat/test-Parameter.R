test_that("Parameters smoke tests", {
    p <- Parameter(name = "intercept", prior = prior_beta(5, 4))

    expected_mu <- 5 / (5 + 4)
    with_mocked_bindings(
        expect_equal(
            initialValues(p),
            expected_mu * 0.5
        ),
        local_rbeta = \(...) 0
    )
    expect_equal(names(p), "intercept")
})

test_that("show() works for Paramneter objects", {
    x <- Parameter(prior_normal(1, 3), "bob", "size1")
    expect_snapshot(print(x))

    x <- Parameter(prior_beta(0.5, 0.2), "var1", "size1")
    expect_snapshot(print(x))

    x <- Parameter(prior_none(), "x", "size1")
    expect_snapshot(print(x))
})
