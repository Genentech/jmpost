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

    } else {

        if (file.exists(system.file("stanparts", file, package = "jmpost"))) {
            absolute_filename <- system.file(
                "stanparts",
                file,
                package = "jmpost"
            )

            out <- readLines(absolute_filename)

        } else {

            out <- filename
        }
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
      msg = "`Functions`, `data`, `parameters`, `transformed_parameters` and `gnenerated_quantities` must be character vectors"
    )

    assert_that(
      is.list(priors),
      is.list(inits),
      length(inits) == length(priors),
      msg = "`Priors` and `inits` must be list of the same length"
    )

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
