my_long_mod <- LongModel(stan = StanModule(functions = "exp_long_functions.stan",
                                           parameters = "exp_long_parameters.stan",
                                           transformed_parameters = "exp_long_transformed_parametes.stan",
                                           generated_quantities = "",
                                           data = "exp_long_data.stan",
                                           priors = list(),
                                           inits = list()))
my_os_mod <- LogLogisticModule()

my_link <- HazardLink(
    parameters = "beta_ttg",
    contribution = "beta_ttg * ttg(phi)",
    stan = StanModule(
        functions = "real ttg(real phi) {phi^2 };"
    )
)

temp_os <- parametrize(osmod = my_os_mod, link = my_link)
