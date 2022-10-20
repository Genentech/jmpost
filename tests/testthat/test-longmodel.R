




test_that("Long model is created as expected", {
    stanobj <- StanModule(functions = "abcdef")
    longmod <- LongModel(stan = stanobj)
    expect_equal(longmod@stan@functions, "abcdef")
    expect_error(getLink(longmod), "link is not defined")
    expect_error(getLinkTTG(longmod), "link is not defined")
    expect_error(getLinkDSLD(longmod), "link is not defined")
})


test_that("Can define our own Link methods and getLink works as expected", {
    stanobj <- StanModule(functions = "abcdef")

    TestLongModel <- setClass(
        Class = "TestLongModel",
        contains = "LongModel"
    )
    setMethod(
        f = "getLinkTTG",
        signature = "TestLongModel",
        definition = function(object) {
            HazardLink(
                parameters = "beta_ttg",
                contribution = "beta_ttg * fun(x)",
                stan = StanModule(functions = "fun(x) 1")
            )
        }
    )
    setMethod(
        f = "getLinkDSLD",
        signature = "TestLongModel",
        definition = function(object) {
            HazardLink(
                parameters = "beta_dlsd",
                contribution = "beta_dsld * fun(y)",
                stan = StanModule(functions = "fun(y) 1")
            )
        }
    )
    tlm <- TestLongModel(stan = stanobj)

    hazlink <- getLink(tlm)
    expect_equal(hazlink@parameters, c("beta_ttg", "beta_dlsd"))
    expect_equal(hazlink@stan@functions, c("fun(x) 1", "fun(y) 1"))

    hazlink2 <- getLink(tlm, selection = "dsld")
    expect_equal(hazlink2@parameters, c("beta_dlsd"))
    expect_equal(hazlink2@stan@functions, c("fun(y) 1"))

    setMethod(
        f = "getLink",
        signature = "TestLongModel",
        definition = function(object) {
            HazardLink(
                parameters = "beta",
                contribution = "beta * fun(z)",
                stan = StanModule(functions = "fun(z) 1")
            )
        }
    )
    hazlink3 <- getLink(tlm)
    expect_equal(hazlink3@parameters, c("beta"))
    expect_equal(hazlink3@stan@functions, c("fun(z) 1"))
})



test_that("Priors of the Long model is replaced ", {
    stanobj <- StanModule(functions = "abcdef", priors = long_prior())
    longmod <- LongModel(stan = stanobj)
    actual_get <- priors(longmod)
    expected_get <- long_prior()

    expect_equal(actual_get, expected_get)


    priors(longmod)["eta_tilde_ks"] <- "normal(0,6)"

    actual_replaced <- priors(longmod)["eta_tilde_ks"]
    expected_replaced <- list(eta_tilde_ks = "normal(0,6)")
    expect_equal(actual_replaced, expected_replaced)
})
