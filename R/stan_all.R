#' StanAll class object
#' @slot functions Character, the functions part of a stan model
#' @slot data Character, the data part of a stan model
#' @slot parameters Character, the parameters part of a stan model
#' @slot transformed_parameters Character, the transformed_parameters part of a stan model
#' @slot model Character, the model part of a stan model
#' @slot prior List, the priors part of a stan model
#' @slot generated_quantities Character, the generated_quantities part of a stan model
#' @slot includes Character
#' @slot inits List with the initial values of the stan model.
#' @exportClass StanModule

stan_module <- setClass(
    "StanModule",
    representation(
        functions = "character",
        data = "character",
        parameters = "character",
        transformed_parameters = "character",
        prior = "list",
        generated_quantities = "character",
        inits = "list"
    )
)


# StanOS: class specifically for overall survival models
#' @exportClass StanOS

.stan_os <- setClass("StanOS",
                     representation(module = "StanModule",
                                    templated = "logical")
)


# StanOS: function for creation of StanOs object
#' @export
stan_os <- function(functions = "os_functions.stan",
                    data = "os_data.stan",
                    parameters = "os_parameters.stan",
                    transformed_parameters = "os_transformed_parameters.stan",
                    prior = os_prior(),
                    generated_quantities = "os_generated_quantities.stan",
                    inits = os_inits,
                    templated = FALSE){


    .stan_os(module = stan_module(functions = source_stan_part(functions),
                                  data = source_stan_part(data),
                                  parameters = source_stan_part(parameters),
                                  transformed_parameters = source_stan_part(transformed_parameters),
                                  prior = prior,
                                  generated_quantities = source_stan_part(generated_quantities),
                                  inits = inits),
             templated = templated)
}





# StanLong: class specifically for longitudinal models
#' @exportClass StanLong

.stan_long <- setClass("StanLong",
    contains = "StanAll"
)


# jmpost: Class containing the output of the model
cmdstan_mod <- R6::R6Class("CmdStanModel")
setOldClass("CmdStanModel")

#' @exportClass JMModel

jm_model <- setClass("JMModel",
                     slots = c(cmdstan_mod = "CmdStanModel"),
                     contains = "StanAll"
)

#' @exportClass HazardLink

hazard_link <- setClass("HazardLink",
                        contains = "StanAll",
                        representation(
                          arguments = "character",
                          calculations = "character",
                          contributions = "character",
                          population_contributions = "character"
                        )
)

#' @exportClass ExponentialLongModel

.exponential_long_model <- setClass("ExponentialLongModel", contains = "StanLong")




