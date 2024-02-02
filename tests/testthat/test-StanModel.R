
test_that("StanModel print function works as expected", {
    x <- StanModel(
        stan = StanModule(),
        parameters = ParameterList(
            Parameter(name = "x", prior = prior_normal(3, 1)),
            Parameter(name = "z", prior = prior_gamma(3, 1))
        ),
        name = "MyModel"
    )
    expect_snapshot(print(x))
})
