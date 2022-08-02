
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

    expect_true(all(sapply(
        c("link_arguments", "link_log_hazard_contribution", "link_arguments_as_par"),
        grepl, os_mod_f@stan@functions
    )) != TRUE)

    expect_true(all(sapply(c("link_parameters"), grepl, os_mod_f@stan@parameters)) != TRUE)

    expect_true(all(sapply(
        c("link_log_surv", "link_log_lik"),
        grepl, os_mod_f@stan@transformed_parameters
    )) != TRUE)

    expect_true(all(sapply(c("link_arguments_as_par"), grepl, os_mod_f@stan@generated_quantities)) != TRUE)
})
