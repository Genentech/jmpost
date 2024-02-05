test_that("LinkRandomSlope smoke tests", {
    linkobject <- LinkRandomSlope(
        link_lm_phi = prior_normal(0, 5)
    )
    iv <- with_mocked_bindings(
        initialValues(linkobject, n_chains = 2),
        local_rnorm = \(...) 5
    )
    expect_equal(
        iv,
        list(
            list("link_lm_phi" = 5 / 2),
            list("link_lm_phi" = 5 / 2)
        )
    )
    expect_true(
        is(as.StanModule(linkobject), "StanModule")
    )
    expect_error(
        LongitudinalRandomSlope(lm_rs_pp = prior_normal(0, 1)),
        "unused argument \\(lm_rs_pp"
    )
})

test_that("Print method for LinkRandomSlope works as expected", {

    expect_snapshot({
        x <- LinkRandomSlope()
        print(x)
    })

    expect_snapshot({
        x <- LinkRandomSlope(link_lm_phi = prior_normal(0, 1))
        print(x)
    })
})
