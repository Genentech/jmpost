
test_that("Print method for SurvivalWeibullPH works as expected", {

    expect_snapshot({
        x <- SurvivalWeibullPH()
        print(x)
    })


    expect_snapshot({
        x <- SurvivalWeibullPH(
            beta = prior_gamma(3, 4),
            gamma = prior_cauchy(0, 1)
        )
        print(x)
    })
})
