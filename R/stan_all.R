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
#' @exportClass StanAll

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


# TemplatedStanOS: class specifically for premature overall survival models
#' @exportClass TemplatedStanOS
#' @export

temp_stan_os <- setClass("TemplatedStanOS",
                         contains = "StanAll"
)

# StanOS: class specifically for overall survival models
#' @exportClass StanOS
#' @export

stan_os <- setClass("StanOS",
    contains = "StanAll"
)

# StanLong: class specifically for longitudinal models
#' @exportClass StanLong
#' @export

.stan_long <- setClass("StanLong",
    contains = "StanAll"
)


# jmpost: Class containing the output of the model
cmdstan_mod <- R6::R6Class("CmdStanModel")
setOldClass("CmdStanModel")

#' @exportClass JMModel
#' @export

jm_model <- setClass("JMModel",
                     slots = c(cmdstan_mod = "CmdStanModel"),
                     contains = "StanAll"
)

#' @exportClass HazardLink
#' @export

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
#' @export

.exponential_long_model <- setClass("ExponentialLongModel", contains = "StanLong")




