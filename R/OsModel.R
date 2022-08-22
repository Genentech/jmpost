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
#' @param model  A prior list with the priors of the model.
#' @param inits  A list with the initial values.
#' @export
LogLogisticModule <- function(functions = "os_functions.stan",
                              data = "os_data.stan",
                              parameters = "os_parameters.stan",
                              transformed_parameters = "os_transformed_parameters.stan",
                              generated_quantities = "os_generated_quantities.stan",
                              priors = os_prior(),
                              model = "target += sum(log_lik);",
                              inits = list()) {
    StanModule(
        functions = functions,
        data = data,
        parameters = parameters,
        transformed_parameters = transformed_parameters,
        generated_quantities = generated_quantities,
        priors = priors,
        model = model,
        inits = inits
    )
}







#' Parametrize TemplatedStanOs object with the selected Hazardlink
#' @param osmod TemplatedStanOs object
#' @param link HazardLink object
#' @export
setGeneric("parametrize", function(osmod, link) {
    standardGeneric("parametrize")
})



setMethod(
    "parametrize",
    signature(osmod = "OsModel", link = "HazardLink"),
    function(osmod, link) {
        newOS <- osmod


        gap_map <- list(
            "<link_arguments>" = paste0("real ", link@parameters, ","),
            "<link_log_hazard_contribution>" = paste0(link@parameters, link@contribution),
            "<link_arguments_as_par>" = paste0(link@parameters, collapse = ","),
            "<link_parameters>" = link@stan@parameters,
            "<link_log_surv>" = paste0(paste0(link@parameters, collapse = ","), ","),
            "<link_log_lik>" = paste0(paste0(link@parameters, collapse = ","), ",")
        )

        slot_map <- c(
            "functions",
            "parameters",
            "transformed_parameters",
            "generated_quantities"
        )

        for (i in slot_map) {
            if (is.na(slot(newOS@stan, i))) next

            for (k in seq_along(gap_map)) {
                char <- slot(newOS@stan, i)

                tmp_char <- gsub(
                    pattern = names(gap_map)[k],
                    replacement = gap_map[k],
                    x = char
                )

                if (i == "functions" & names(gap_map)[k] == "<link_arguments>") {
                    tmp_char <- paste0(tmp_char, "\n ", link@stan@functions)
                }

                slot(newOS@stan, i) <- tmp_char
            }
        }



        temp_obj <- merge(osmod@stan, link@stan)
        newOS@stan@priors <- temp_obj@priors
        newOS@stan@inits <- temp_obj@inits



        newOS
    }
)
