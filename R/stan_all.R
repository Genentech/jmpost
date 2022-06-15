# StanAll: class containing all sections for a stan model
stan_all <- setClass(
  "StanAll",
  representation(
    functions = "character",
    data = "character",
    parameters = "character",
    transformed_parameters = "character",
    model = "character",
    prior = "list",
    generated_quantities = "character",
    includes = "character",
    inits = "list"
  )
)

# StanOS: class specifically for overall survival models
stan_os <- setClass("StanOS",
  contains = "StanAll"
)

# StanLong: class specifically for longitudinal models
.stan_long <- setClass("StanLong",
  contains = "StanAll"
)


# jmpost: Class containing the output of the model
cmdstan_mod <- R6::R6Class("CmdStanModel")
setOldClass("CmdStanModel")


jm_model <- setClass("JMModel",
                     slots = c(cmdstan_mod = "CmdStanModel"),
                     contains = "StanAll"
)


cmdstan_fit <- R6::R6Class("CmdStanMCMC")
setOldClass("CmdStanMCMC")


jm_post_class <- setClass("JMpost",
                          slots = c(cmdstan_fit = "CmdStanMCMC", data_list = "list"),
                          contains = "JMModel"
)

jm_data <- setClass(
  "JMdata",
  representation(
    data_sld = "data.frame",
    data_os = "data.frame",
    data = "list",
    vars = "list"
  )
)






hazard_link <- setClass("HazardLink",
                        contains = "StanAll",
                        representation(
                          arguments = "character",
                          calculations = "character",
                          contributions = "character",
                          population_contributions = "character"
                        )
)


.exponential_long_model <- setClass("ExponentialLongModel", contains = "StanLong")



.gauss_legendre <- setClass(
  "gauss_legendre",
  representation(values = "list")
)
