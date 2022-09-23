test_that("Priors of the stanmodule is replaced ", {
        stan = StanModule(
            functions = "abcdef",
            priors=list(mu_kg= "lognormal(-0.36,1);")
        )

    actual_get<- prior(stan)
    expected_get<-list(mu_kg= "lognormal(-0.36,1);")

    expect_equal(actual_get, expected_get)

    prior(stan)["mu_kg"]<-"lognormal(-0.24,1);"
    actual_replaced<- prior(stan)["mu_kg"]
    expected_replaced<-list(mu_kg="lognormal(-0.24,1);")
    expect_equal(actual_replaced, expected_replaced)

})


test_that("Priors of the Long model is replaced ", {
    stanobj <- StanModule(functions = "abcdef",priors=long_prior())
    longmod <- LongModel(stan = stanobj)
    actual_get<- prior(longmod)
    expected_get<-long_prior()

    expect_equal(actual_get, expected_get)


    prior(longmod)["eta_tilde_ks"]<-"normal(0,6)"

    actual_replaced<- prior(longmod)["eta_tilde_ks"]
    expected_replaced<-list(eta_tilde_ks="normal(0,6)")
    expect_equal(actual_replaced, expected_replaced)

})


test_that("Priors of the OS model is replaced ", {
    osmod <- OsModel(stan = StanModule(functions = "Os Functions",priors=os_prior()))

    actual_get<- prior(osmod)
    expected_get<-os_prior()

    expect_equal(actual_get, expected_get)


    prior(osmod)["p"]<-"gamma(2, 1);"

    actual_replaced<- prior(osmod)["p"]
    expected_replaced<-list(p="gamma(2, 1);")
    expect_equal(actual_replaced, expected_replaced)

})


test_that("Priors of the Hazardlink is replaced ", {
    hazlink<-HazardLink(
        parameters = "beta_ttg",
        contribution = "beta_ttg * ttg(phi)",
        stan = StanModule(
            functions = "real ttg( real phi) { phi^2 };",
            priors=list(phi= "lognormal(0,0.5)")
        )
    )

    actual_get<- prior(hazlink)
    expected_get<-list(phi= "lognormal(0,0.5)")

    expect_equal(actual_get, expected_get)


    prior(hazlink)["phi"]<-"lognormal(0,1);"
    actual_replaced<- prior(hazlink)["phi"]
    expected_replaced<-list(phi="lognormal(0,1);")
    expect_equal(actual_replaced, expected_replaced)

})




