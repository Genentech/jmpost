my_long_mod <- LongModel(stan = StanModule(functions = "exp_long_functions.stan",
                                           data = "exp_long_data.stan",
                                           priors = long_prior(),
                                           inits = list(),
                                           parameters = "exp_long_parameters.stan",
                                           transformed_parameters = "exp_long_transformed_parametes.stan"))

my_temp_os <- LogLogisticOs()

my_link <- HazardLink(
    parameters = "beta_ttg",
    contribution = "* rep_matrix(ttg(psi_ks, psi_kg, psi_phi), rows(time))",
    stan = StanModule( )
)

my_os <- parametrize(osmod = my_temp_os, link = my_link)


jm_complete(Long = my_long_mod, Os = my_os)@priors


cmdstanr::write_stan_file(
    as.character(jm_complete(Long = my_long_mod, Os = my_os)),
    basename = "stan_model.stan",
    dir = paste0(system.file(package = "jmpost"), "/stanmodels")
)


jmModel(Long = my_long_mod, Os = my_os)
