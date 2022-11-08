

test_that("Basic HazardLink usage works as exepected", {

    actual <- HazardLink(
        parameters = "beta_ttg",
        contribution = "beta_ttg * ttg(phi)",
        stan = StanModule(
            functions = "real ttg( real phi) { phi^2 };"
        )
    )

    expect_equal(actual@parameters, "beta_ttg")
    expect_equal(actual@stan@functions, "real ttg( real phi) { phi^2 };")
    expect_equal(actual@stan@parameters, "real beta_ttg;")

})


test_that("HazardLinks can merge together", {

    h1 <- HazardLink(
        parameters = "beta_dsld",
        contribution = "beta_dsld * dsld(phi)",
        stan = StanModule(
            functions = "real dsld( real phi) { phi^2 };"
        )
    )

    h2 <- HazardLink(
        parameters = "beta_ttg",
        contribution = "beta_ttg * ttg(phi)",
        stan = StanModule(
            functions = "real ttg( real phi) { phi^2 };"
        )
    )

    actual <- merge(h1, h2)

    expect_equal(actual@parameters, c("beta_dsld", "beta_ttg"))
    expect_equal(
        actual@stan@functions,
        c(
            "real dsld( real phi) { phi^2 };",
            "real ttg( real phi) { phi^2 };"
        )
    )
    expect_equal(actual@contribution, "beta_dsld * dsld(phi) + beta_ttg * ttg(phi)")
    expect_equal(actual@stan@parameters, c("real beta_dsld;", "real beta_ttg;"))

})



test_that("Priors of the Hazardlink is replaced ", {
    hazlink<-HazardLink(
        parameters = "beta_ttg",
        contribution = "beta_ttg * ttg(phi)",
        stan = StanModule(
            functions = "real ttg( real phi) { phi^2 };",
            priors=list(beta_ttg= "lognormal(0,0.5)")
        )
    )

    actual_get<- priors(hazlink)
    expected_get<-list(beta_ttg= "lognormal(0,0.5)")

    expect_equal(actual_get, expected_get)


    priors(hazlink)["beta_ttg"]<-"lognormal(0,1);"
    actual_replaced<- priors(hazlink)["beta_ttg"]
    expected_replaced<-list(beta_ttg="lognormal(0,1);")

    expect_equal(actual_replaced, expected_replaced)

})




test_that("initial value of the Hazardlink is replaced ", {
    hazlink <- HazardLink(
        parameters = "beta_ttg",
        contribution = "beta_ttg * ttg(phi)",
        stan = StanModule(
            functions = "real ttg( real phi) { phi^2 };",
            inits = list(beta_ttg = 0.01),
            priors = list(beta_ttg = "lognormal(0,0.5)")

        )
    )

    actual_get <- inits(hazlink)
    expected_get <- list(beta_ttg = 0.01)

    expect_equal(actual_get, expected_get)


    inits(hazlink)["beta_ttg"] <- 0.06
    actual_replaced <- inits(hazlink)["beta_ttg"]
    expected_replaced <- list(beta_ttg = 0.06)

    expect_equal(actual_replaced, expected_replaced)

})

