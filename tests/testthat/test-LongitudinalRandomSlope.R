
test_that("Print method for LongitudinalRandomSlope works as expected", {

    expect_snapshot({
        x <- LongitudinalRandomSlope()
        print(x)
    })

    expect_snapshot({
        x <- LongitudinalRandomSlope(
            intercept = prior_normal(0, 1),
            sigma = prior_gamma(2, 1)
        )
        print(x)
    })
})
