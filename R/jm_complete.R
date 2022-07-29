#' @export

setGeneric("jm_complete", def = function(Long, Os) {
    standardGeneric("jm_complete")
})


# prior_char: function for converting a list with the priors to a character vector
convert_to_model <- function(x) {
    prior <- paste(paste(paste0(names(x), "~", unlist(x)), collapse = ";\n"), ";")
    paste(prior, "target+=sum(log_lik);\n")
}


setMethod("jm_complete",
    signature(Long = "LongModel", Os = "OsModel"),
    value = "StanModule",
    def = function(Long, Os) {
        priors <- append(Long@stan@priors, Os@stan@priors)
        priors <- priors[!duplicated(names(priors))]

        StanModule(
            functions = paste0(Long@stan@functions,
                Os@stan@functions,
                collapse = "\n"
            ),
            data = paste0(Long@stan@data,
                Os@stan@data,
                collapse = "\n"
            ),
            priors = priors,
            parameters = paste0(Long@stan@parameters,
                Os@stan@parameters,
                collapse = "\n"
            ),
            transformed_parameters = paste0(Long@stan@transformed_parameters,
                Os@stan@transformed_parameters,
                collapse = "\n"
            ),
            generated_quantities = paste0(Long@stan@generated_quantities,
                Os@stan@generated_quantities,
                collapse = "\n"
            ),
        )
    }
)
