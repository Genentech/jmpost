#' @export
#' @param Long A longitudinal object of type LongMod
#' @param Os An overall survival object of type OsMod
setGeneric("jm_complete", def = function(Long, Os) {
    standardGeneric("jm_complete")
})



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
            inits = append(Long@stan@inits, Os@stan@inits)
        )
    }
)
