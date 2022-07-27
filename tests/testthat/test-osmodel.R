
test_that("parametrize filled all the gaps", {

    os_mod <- LogLogisticOs()
    h_link <- HazardLink(stan = StanModule(functions = "bla",
                                           data = "bla",
                                           parameters = "bla",
                                           transformed_parameters = "bla",
                                           generated_quantities = "bla",
                                           priors = list(),
                                           inits = list()),
                         parameters = "bla",
                         contribution = "bla")
    os_mod_f <- parametrize(os_mod, h_link)

    all(sapply(c("<", ">"), grepl, os_mod_f@stan@parameters)) != TRUE
    all(sapply(c("<", ">"), grepl, os_mod_f@stan@transformed_parameters)) != TRUE
    all(sapply(c("<", ">"), grepl, os_mod_f@stan@generated_quantities)) != TRUE
})
