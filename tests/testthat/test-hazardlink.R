

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






