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
        stan = "StanModule"
    )
)



#' LogLogisticModule
#'
#' Log logistic module helper function.
#' @param functions A stan code including the functions section of the model.
#' @param data  A stan code including the data section of the model.
#' @param parameters  A stan code including the parameters section of the model.
#' @param transformed_parameters  A stan code including the transformed parameters section of the model.
#' @param generated_quantities  A stan code including the generated quantities section of the model.
#' @param priors  A prior list with the priors of the model.
#' @param inits  A list with the initial values.
#' @export
LogLogisticModule <- function(functions = "os_functions.stan",
                              data = "os_data.stan",
                              parameters = "os_parameters.stan",
                              transformed_parameters = "os_transformed_parameters.stan",
                              generated_quantities = "os_generated_quantities.stan",
                              priors = os_prior(),
                              inits = list()) {
    StanModule(
        functions = functions,
        data = data,
        parameters = parameters,
        transformed_parameters = transformed_parameters,
        generated_quantities = generated_quantities,
        priors = priors,
        inits = inits
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


setMethod(
    "parametrize",
    signature(osmod = "OsModel", link = "HazardLink"),
    function(osmod, link) {
        newOS <- osmod

        newOS@stan@functions <- gsub(
            "<link_arguments>",
            paste0("real ", link@parameters, ","),
            osmod@stan@functions
        ) |>
            paste0("\\n ", link@stan@functions)

        newOS@stan@functions <- gsub(
            "<link_log_hazard_contribution>",
            paste0(link@parameters, link@contribution),
            newOS@stan@functions
        )
        newOS@stan@functions <- gsub(
            "<link_arguments_as_par>",
            paste0(link@parameters),
            newOS@stan@functions
        )



        temp_obj <- merge(osmod@stan, link@stan)
        newOS@stan@priors <- temp_obj@priors
        newOS@stan@inits <- temp_obj@inits

        newOS@stan@parameters <- gsub(
            pattern = "<link_parameters>",
            replacement = link@stan@parameters,
            osmod@stan@parameters
        )


        newOS@stan@transformed_parameters <- gsub(
            pattern = "<link_log_surv>",
            replacement = paste0(paste0(link@parameters, collapse = ","), ","),
            osmod@stan@transformed_parameters
        )
        newOS@stan@transformed_parameters <- gsub(
            pattern = "<link_log_lik>",
            replacement = paste0(paste0(link@parameters, collapse = ","), ","),
            newOS@stan@transformed_parameters
        )


        newOS@stan@generated_quantities <- gsub(
            pattern = "<link_arguments_as_par>",
            replacement = paste0(paste0(link@parameters, collapse = ","), ","),
            osmod@stan@generated_quantities
        )



        newOS
    }
)
