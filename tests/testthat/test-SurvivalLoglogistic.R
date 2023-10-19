
test_that("Print method for SurvivalLogLogistic works as expected", {

    expect_snapshot({
        x <- SurvivalLogLogistic()
        print(x)
    })

    expect_snapshot({
        x <- SurvivalLogLogistic(
            beta = prior_gamma(3, 4, init = 10),
            p = prior_cauchy(0, 1)
        )
        print(x)
    })
})
