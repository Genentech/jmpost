# StanOS: class specifically for overall survival models
#' @exportClass StanOS

.stan_os <- setClass("StanOS",
                     representation(module = "StanModule",
                                    templated = "logical")
)


# StanOS: function for creation of StanOs object
#' Creator function for overall survival submodel
#' @param functions Character, the functions part of a stan model
#' @param data Character, the data part of a stan model
#' @param parameters Character, the parameter part of a stan model
#' @param transformed_parameters Character, the transformed_parameters part of a stan model
#' @param prior List, the priors part of a stan model
#' @param generated_quantities Character, the generated_quantities part of a stan model
#' @param templated Logical, defines whether the StanOs object is ready to be merged with the longitudinal model
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



