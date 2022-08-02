#' Title - TODO
#'
#' Description - TODO
#'
#' @slot stan TODO
#' @slot templated Logical. Defines whether the OsModel object needs to be completed
#' @export
OsModel <- setClass(
    Class = "OsModel",
    representation = list(
        stan = "StanModule",
        templated = "logical"
    )
)



#' LogLogisticModule
#'
#' Log logistic module helper function.
#' @param functions
#' @param data
#' @param parameters
#' @param transformed_parameters
#' @param generated_quantities
#' @param priors
#' @param inits
#' @param templated
#' @export
LogLogisticModule <- function(functions = "os_functions.stan",
                              data = "os_data.stan",
                              parameters = "os_parameters.stan",
                              transformed_parameters = "os_transformed_parameters.stan",
                              generated_quantities = "os_generated_quantities.stan",
                              priors = os_prior(),
                              inits = list(),
                              templated = TRUE) {
    st_mod <- StanModule(
            functions = functions,
            data = data,
            parameters = parameters,
            transformed_parameters = transformed_parameters,
            generated_quantities = generated_quantities,
            priors = priors,
            inits = inits
        )

    OsModel(
        stan = st_mod,
        templated = templated
    )
}







#' Parametrize TemplatedStanOs object with the selected Hazardlink
#' @param osmod TemplatedStanOs object
#' @param link HazardLink object
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @export
setGeneric("parametrize", function(osmod, link) {
    standardGeneric("parametrize")
})





# To be updated
setMethod(
    "parametrize",
    signature(osmod = "OsModel", link = "HazardLink"),
    function(osmod, link) {
        newOS <- osmod@stan

        newOS@stan@functions <- str_replace_all(
            string = osmod@stan@functions,
            pattern = "<link_arguments>",
            replacement = paste0("real ", link@parameters, ",")
        ) |>
            paste0("\\n ", link@stan@functions) |>
            str_replace(
                pattern = "<link_log_hazard_contribution>",
                replacement = paste0(link@parameters, link@contribution)
            ) |>
            str_replace(
                pattern = "<link_arguments_as_par>",
                replacement = paste0(link@parameters)
            )


        temp_obj <- merge(osmod@stan, link@stan)
        newOS@stan@priors <- temp_obj@stan@priors
        newOS@stan@inits <- temp_obj@stan@inits

        newOS@stan@parameters <- str_replace(
            string = osmod@stan@parameters,
            pattern = "<link_parameters>",
            replacement = link@stan@parameters
        )

        newOS@stan@transformed_parameters <- str_replace(
            string = osmod@stan@transformed_parameters,
            pattern = "<link_log_surv>",
            replacement = paste0(paste0(link@parameters, collapse = ","), ",")
        ) |>
            str_replace(
                pattern = "<link_log_lik>",
                replacement = paste0(paste0(link@parameters, collapse = ","), ",")
            )

        newOS@stan@generated_quantities <- str_replace_all(
            string = osmod@stan@generated_quantities,
            pattern = "<link_arguments_as_par>",
            replacement = paste0(paste0(link@parameters, collapse = ","), ",")
        )


        newOS
    }
)
