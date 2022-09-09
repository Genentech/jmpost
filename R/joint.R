
#' @rdname joint
setMethod("joint",
    signature(long = "LongModel", os = "OsModel"),
    value = "StanModule",
    def = function(long, os) {
        priors <- append(long@stan@priors, os@stan@priors)
        priors <- priors[!duplicated(names(priors))]

        StanModule(
            functions = paste0(long@stan@functions,
                os@stan@functions,
                collapse = "\n"
            ),
            data = paste0(long@stan@data,
                os@stan@data,
                collapse = "\n"
            ),
            priors = priors,
            model = paste0(long@stan@model,
                           os@stan@model,
                           collapse = "\n"
            ),
            parameters = paste0(long@stan@parameters,
                os@stan@parameters,
                collapse = "\n"
            ),
            transformed_parameters = paste0(long@stan@transformed_parameters,
                os@stan@transformed_parameters,
                collapse = "\n"
            ),
            generated_quantities = paste0(long@stan@generated_quantities,
                os@stan@generated_quantities,
                collapse = "\n"
            ),
            inits = append(long@stan@inits, os@stan@inits)
        )
    }
)
