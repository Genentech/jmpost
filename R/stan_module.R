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



#' source_stan_part exports a .stan file as a character
#' @param filename The name of the .stan file in the directory of the package.
#' @export
source_stan_part <- function(filename) {
    if (file.exists(filename)) {
        absolute_filename <- filename
    } else {
        absolute_filename <- system.file(
            "stanparts",
            filename,
            package = "jmpost"
        )
    }

    readLines(absolute_filename)
}


#' @importFrom assertthat assert_that
#' @param functions TODO
#' @param data TODO
#' @param parameters TODO
#' @param transformed_parameters TODO
#' @param prior TODO
#' @param generated_quantities TODO
#' @param inits TODO
#' @export
setMethod(
    f = "initialize",
    signature = "StanModule",
    definition = function(.Object, ..., functions, data,
                          parameters, transformed_parameters,
                          prior, generated_quantities, inits) {
        assert_that(
            is.character(functions),
            length(functions) == 1,
            is.character(data),
            length(data) == 1,
            is.character(parameters),
            length(parameters) == 1,
            is.character(transformed_parameters),
            length(transformed_parameters) == 1,
            is.character(gnenerated_quantities),
            length(gnenerated_quantities) == 1,
            msg = "`Functions`, `data`, `parameters`, `transformed_parameters` and `gnenerated_quantities` must be length 1 character vectors"
        )

        assert_that(
            is.list(prior),
            is.list(inits),
            length(inits) == length(prior),
            msg = "`Prior` and `inits` must be list of the same length"
        )

        callNextMethod(
            .Object,
            ...,
            functions = source_stan_part(functions),
            data = source_stan_part(data),
            parameters = source_stan_part(parameters),
            transformed_parameters = source_stan_part(transformed_parameters),
            prior = prior,
            generated_quantities = source_stan_part(generated_quantities),
            inits = inits
        )
    }
)
