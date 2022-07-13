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
StanModule <- setClass(
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


