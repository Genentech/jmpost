#' StanAll class object
#' @slot functions Character, the functions part of a stan model.
#' @slot data Character, the data part of a stan model.
#' @slot parameters Character, the parameters part of a stan model.
#' @slot transformed_parameters Character, the transformed_parameters part of a stan model.
#' @slot model Character, the model part of a stan model.
#' @slot priors List, the priors part of a stan model.
#' @slot generated_quantities Character, the generated_quantities part of a stan model.
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
        priors = "list",
        generated_quantities = "character",
        inits = "list"
    )
)



#' read_stan_part returns stan code as a character.
#' @param file Character, either the absolute path of a stan file, or the name of the stan file in the package directory or the stan code as a string.
#' @export
read_stan_part <- function(file) {
    if (file.exists(file)) {
        absolute_filename <- file
        out <- readLines(absolute_filename)
    } else if (file.exists(system.file("stanparts", file, package = "jmpost"))) {
        absolute_filename <- system.file(
            "stanparts",
            file,
            package = "jmpost"
        )

        out <- readLines(absolute_filename)
    } else {
        out <- file
    }

    return(out)
}


#' @importFrom assertthat assert_that
#' @param functions TODO
#' @param data TODO
#' @param parameters TODO
#' @param transformed_parameters TODO
#' @param priors TODO
#' @param generated_quantities TODO
#' @param inits TODO
#' @export
setMethod(
    f = "initialize",
    signature = "StanModule",
    definition = function(.Object, ..., functions, data,
                          parameters, transformed_parameters,
                          priors, generated_quantities, inits) {
        assert_that(
            is.character(functions),
            is.character(data),
            is.character(parameters),
            is.character(transformed_parameters),
            is.character(generated_quantities),
            msg = "`Functions`, `data`, `parameters`, `transformed_parameters` and `generated_quantities` must be character vectors"
        )

        assert_that(
            is.list(priors),
            is.list(inits),
            length(inits) == length(priors),
            msg = "`Priors` and `inits` must be list of the same length"
        )

        if (length(functions) > 1) {
            functions <- paste_str(functions)
        }
        if (length(data) > 1) {
            data <- paste_str(data)
        }
        if (length(parameters) > 1) {
            parameters <- paste_str(parameters)
        }
        if (length(transformed_parameters) > 1) {
            transformed_parameters <- paste_str(transformed_parameters)
        }
        if (length(generated_quantities) > 1) {
            generated_quantities <- paste_str(generated_quantities)
        }


        callNextMethod(
            .Object,
            ...,
            functions = read_stan_part(functions),
            data = read_stan_part(data),
            parameters = read_stan_part(parameters),
            transformed_parameters = read_stan_part(transformed_parameters),
            priors = priors,
            generated_quantities = read_stan_part(generated_quantities),
            inits = inits
        )
    }
)


#' paste_vector Returns a single string from a vector of strings
#' @param vec A vector of stings representing stan code.
#' @export
paste_str <- function(vec) {

    # check if all elements of vector include ";"
    if (all(grepl(";", vec, fixed = TRUE) == TRUE)) {
        paste0(paste0(vec, collapse = "\\n "), "\\n")
    } else if (all(grepl(";", vec, fixed = TRUE) == FALSE)) {
        paste0(paste0(vec, collapse = ";\\n "), ";\\n")
    } else {
        stop("Remove all semicolons")
    }
}
