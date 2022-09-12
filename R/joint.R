
#' @rdname merge
setMethod("merge",
    signature(x = "LongModel", y = "OsModel"),
    value = "StanModule",
    def = function(x, y) {
        priors <- append(x@stan@priors, y@stan@priors)
        priors <- priors[!duplicated(names(priors))]

        StanModule(
            functions = paste0(x@stan@functions,
                y@stan@functions,
                collapse = "\n"
            ),
            data = paste0(x@stan@data,
                y@stan@data,
                collapse = "\n"
            ),
            priors = priors,
            model = paste0(x@stan@model,
                           y@stan@model,
                           collapse = "\n"
            ),
            parameters = paste0(x@stan@parameters,
                y@stan@parameters,
                collapse = "\n"
            ),
            transformed_parameters = paste0(x@stan@transformed_parameters,
                y@stan@transformed_parameters,
                collapse = "\n"
            ),
            generated_quantities = paste0(x@stan@generated_quantities,
                y@stan@generated_quantities,
                collapse = "\n"
            ),
            inits = append(x@stan@inits, y@stan@inits)
        )
    }
)
