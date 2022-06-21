#' @export

setGeneric("jm_complete", def = function(object1, object2) {
  standardGeneric("jm_complete")
})


# prior_char: function for converting a list with the priors to a character vector
convert_to_model <- function(x) {
  prior <- paste(paste(paste0(names(x), "~", unlist(x)), collapse = ";\n"), ";")
  paste(prior, "target+=sum(log_lik);\n")
}


setMethod("jm_complete",
  signature(object1 = "StanAll", object2 = "StanAll"),
  value = "StanAll",
  def = function(object1, object2) {
    priors <- append(object1@prior, object2@prior)
    priors <- priors[!duplicated(names(priors))]

    stan_all(
      functions = paste0(paste0(object1@functions, collapse = "\n"),
                         paste0(object2@functions, collapse = "\n")),
      data = paste0(paste0(object1@data, collapse = "\n"),
                    paste0(object2@data, collapse = "\n")),
      prior = priors,
      model = convert_to_model(priors),
      parameters = paste0(paste0(object1@parameters, collapse = "\n"),
                          paste0(object2@parameters, collapse = "\n")),
      transformed_parameters = paste0( paste0(object1@transformed_parameters, collapse = "\n"),
                                       paste0(object2@transformed_parameters, collapse = "\n")),
      generated_quantities = paste0( paste0(object1@generated_quantities, collapse = "\n"),
                                     paste0(object2@generated_quantities, collapse = "\n")),
      includes = unique(c(object1@includes, object2@includes)),
      inits = append(object1@inits, object2@inits)
    )
  }
)
