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
        templated = TRUE
    )
)


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
        newOS <- OsModel(stan = StanModule(
            functions = osmod@stan@functions,
            data = osmod@stan@data,
            parameters = osmod@stan@data,
            transformed_parameters = osmod@stan@transformed_parameters,
            priors = osmod@stan@priors,
            generated_quantities = osmod@stan@generated_quantities,
            inits = osmod@stan@inits
        ))

        newOS@stan@functions <- str_replace_all(
            string = osmod@stan@functions,
            pattern = "<link_arguments>",
            replacement = paste0("real ", link@parameter, ",")
        )

        newOS@stan@functions <- paste0(
            newOS@stan@functions, "\\n", link@stan@functions
        )

        newOS@stan@functions <- str_replace(
            pattern = "<link_log_hazard_contribution>",
            replacement = paste0(link@parameter,  link@contribution)
        )
        newOS@stan@functions <- str_replace(
            pattern = "<link_arguments_as_par>",
            replacement = str_remove_all(link@arguments, "real")
        )
        newOS@stan@functions <- str_replace(
            pattern = "<Link_pop_log_haz>",
            replacement = link@stan@generated_quantities
        )

        newOS@stan@prior <- append(osmod@stan@prior, link@stan@prior)
        newOS@stan@inits <- append(osmod@stan@inits, link@stan@inits)

        newOS@stan@parameters <- str_replace(
            string = osmod@stan@parameters,
            pattern = "<link_parameters>",
            replacement = link@parameters
        )

        newOS@stan@transformed_parameters <- str_replace(
            string = osmod@stan@transformed_parameters,
            pattern = "<link_log_surv>",
            replacement = paste0(paste0(link@parameter,collapse = ","), ",")
        )
        newOS@stan@transformed_parameters <- str_replace(
            pattern = "<link_log_lik>",
            replacement = paste0(paste0(link@parameter,collapse = ","), ",")
        )

        newOS@stan@generated_quantities <- str_replace_all(
            string = osmod@stan@generated_quantities,
            pattern = "<link_arguments_as_par>",
            replacement = paste0(paste0(link@parameter,collapse = ","), ",")
        )


        newOS
    }
)
