
test_that("parametrize filled all the gaps", {
    os_mod <- LogLogisticOs()
    h_link <- HazardLink(
        stan = StanModule(
            functions = "bla",
            data = "bla",
            parameters = "bla",
            transformed_parameters = "bla",
            generated_quantities = "bla",
            priors = list(),
            inits = list()
        ),
        parameters = "bla",
        contribution = "bla"
    )
    os_mod_f <- parametrize(os_mod, h_link)

    expect_false(all(sapply(
        c("link_arguments", "link_log_hazard_contribution", "link_arguments_as_par"),
        grepl, os_mod_f@stan@functions
    )))

    expect_false(all(sapply(c("link_parameters"), grepl, os_mod_f@stan@parameters)))

    expect_false(all(sapply(
        c("link_log_surv", "link_log_lik"),
        grepl, os_mod_f@stan@transformed_parameters
    )))

    expect_false(all(sapply(c("link_arguments_as_par"), grepl, os_mod_f@stan@generated_quantities)))
})



test_that("merge is working as expected", {
    os_mod <- OsModel(stan = StanModule(
        functions = " bla <link_arguments> bla <link_log_hazard_contribution> bla <link_arguments_as_par>",
        data = "bla",
        parameters = "bla <link_parameters>",
        transformed_parameters = "bla <link_log_surv> bla <link_log_lik>",
        generated_quantities = "bla <link_arguments_as_par>"
    ))
    h_link <- HazardLink(
        stan = StanModule(
            functions = "function",
            data = "data",
            parameters = "parameters",
            transformed_parameters = "transformed_parameters",
            generated_quantities = "generated_quantities",
            priors = list(),
            inits = list()
        ),
        parameters = "beta",
        contribution = "contribution"
    )
    actual <- parametrize(os_mod, h_link)
    expected <- OsModel(stan = StanModule(
        functions = " bla real beta, bla betacontribution bla beta\n function",
        data = "bla",
        parameters = "bla parameters",
        transformed_parameters = "bla beta, bla beta,",
        priors = list(),
        generated_quantities = "bla beta",
        inits = list()
    ))
    expect_equal(actual, expected)
})


expect_snapshot(
    LogLogisticModule(),
    variant = "LogLogisticModule"
)



test_that("parametrize combines functions as expected", {
    os_mod <- OsModel(stan = StanModule(functions = "Os Functions"))
    h_link <- HazardLink(
        stan = StanModule(
            functions = "functions"
        ),
        parameters = "bla",
        contribution = "bla"
    )
    actual <- parametrize(os_mod, h_link)
    expected <- OsModel(stan = StanModule(functions = "Os Functions\n functions"))

    expect_equal(actual, expected)
})


test_that("Priors of the OS model is replaced ", {
    osmod <- OsModel(stan = StanModule(functions = "Os Functions", priors = os_prior()))

    actual_get <- priors(osmod)
    expected_get <- os_prior()

    expect_equal(actual_get, expected_get)


    priors(osmod)["p"] <- "gamma(2, 1);"

    actual_replaced <- priors(osmod)["p"]
    expected_replaced <- list(p = "gamma(2, 1);")
    expect_equal(actual_replaced, expected_replaced)
})
