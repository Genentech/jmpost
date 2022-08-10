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

        newOS@stan@functions <- gsub("<link_arguments>",
                                     paste0("real ", link@parameters, ","),
                                     osmod@stan@functions ) |>
            paste0("\\n ", link@stan@functions) |>
            gsub("<link_log_hazard_contribution>",
                 paste0(link@parameters, link@contribution),
                 .) |>
            gsub("<link_arguments_as_par>",
                 paste0(link@parameters),
                 .)



        temp_obj <- merge(osmod@stan, link@stan)
        newOS@stan@priors <- temp_obj@stan@priors
        newOS@stan@inits <- temp_obj@stan@inits

        newOS@stan@parameters <- gsub(pattern = "<link_parameters>",
                                      replacement = link@stan@parameters,
                                      osmod@stan@parameters)


        newOS@stan@transformed_parameters <- gsub(pattern = "<link_log_surv>",
                                                  replacement = paste0(paste0(link@parameters, collapse = ","), ","),
                                                  osmod@stan@transformed_parameters) |>
            gsub( pattern = "<link_log_lik>",
                  replacement = paste0(paste0(link@parameters, collapse = ","), ","),
                  .)


        newOS@stan@generated_quantities <- gsub(pattern = "<link_arguments_as_par>",
                                                replacement = paste0(paste0(link@parameters, collapse = ","), ","),
                                                osmod@stan@generated_quantities)



        newOS
    }
)
