test_that("Parameters smoke tests", {
    p <- Parameter(name = "intercept", prior = prior_beta(5, 4))
    expect_equal(initialValues(p), 5 / (5 + 4))
    expect_equal(names(p), "intercept")
    p <- Parameter(name = "myp", prior = prior_beta(5, 4, init = 9))
    expect_equal(initialValues(p), 9)
    expect_equal(names(p), "myp")
})

test_that("show() works for Paramneter objects", {
    x <- Parameter(prior_normal(1, 3, 4), "bob", "ben")
    expect_snapshot_output(print(x))

    x <- Parameter(prior_beta(0.5, 0.2), "var1", "var2")
    expect_snapshot_output(print(x))

    x <- Parameter(prior_none(4), "x", "y")
    expect_snapshot_output(print(x))
})
