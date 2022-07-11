# StanLong: class specifically for longitudinal models
#' @exportClass StanLong

.stan_long <- setClass("StanLong",
                       representation(module = "StanModule",
                                      templated = "logical")
)

# StanLong: function for creation of StanOs object
#' Creator function for the longitudinal submodel
#' @param functions Character, the functions part of a stan model
#' @param data Character, the data part of a stan model
#' @param parameters Character, the parameter part of a stan model
#' @param transformed_parameters Character, the transformed_parameters part of a stan model
#' @param prior List, the priors part of a stan model
#' @param generated_quantities Character, the generated_quantities part of a stan model
#' @param templated Logical, defines whether the StanLong object is ready to be merged with the overall survival model
#' @export
stan_long <- function(functions = "exp_long_functions.stan",
                    data = "exp_long_data.stan",
                    parameters = "exp_long_parameters.stan",
                    transformed_parameters = "exp_long_transformed_parametes.stan",
                    prior = long_prior(),
                    generated_quantities = "os_generated_quantities.stan",
                    inits = long_inits,
                    templated = FALSE){


    .stan_long(module = stan_module(functions = source_stan_part(functions),
                                  data = source_stan_part(data),
                                  parameters = source_stan_part(parameters),
                                  transformed_parameters = source_stan_part(transformed_parameters),
                                  prior = prior,
                                  generated_quantities = source_stan_part(generated_quantities),
                                  inits = inits),
             templated = templated)
}
