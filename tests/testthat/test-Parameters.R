test_that("Parameters smoke tests", {
    p <- Parameter(name = "intercept", prior = prior_beta(5, 4))
    expect_equal(initialValues(p), 5 / (5 + 4))
    expect_equal(names(p), "intercept")
    p <- Parameter(name = "myp", prior = prior_beta(5, 4, init = 9))
    expect_equal(initialValues(p), 9)
    expect_equal(names(p), "myp")
})
