#' @slot module
#' @slot par_name name of the parameter
#' @slot contributions Function for the log hazard calculation
#' @exportClass HazardLink

hazard_link <- setClass(
    "HazardLink",
    representation(
        module = "StanModule",
        par_name = "character",
        contributions = "character"
    )
)




#' Parametrize TemplatedStanOs object with the selected Hazardlink
#' @param LongMod
#' @param select
#' @export


setGeneric("get_link", function(LongMod, select) {
    standardGeneric("get_link")
})

setMethod(
    "get_link",
    signature(LongMod = "StanLong"),
    function(LongMod, select) {


        link_out <- hazard_link(module = stan_module(functions = "",
                                                     prior = list(),
                                                     inits = list()),
                                par_name = NA_character_,
                                contributions = ""
        )

        if (length(select) < 2) {
            tmp_module <- stan_module(
                functions = source_stan_part(paste0(select, "_functions.stan")),
                prior = list("normal(0,5)"),
                inits = list(0)
            )

            hazard_link(
                module = tmp_module,
                par_name = select,
                contributions = paste0(select, "*", select, link_contribution_prep(tmp_module))
            )
        } else {
            for (i in 1:length(select)) {
                tmp_module <- stan_module(
                    functions = source_stan_part(paste0(select[i], "_functions.stan")),
                    prior = list("normal(0,5)"),
                    inits = list(0)
                )
                names(tmp_module@prior) <- select[i]
                names(tmp_module@inits) <- select[i]

                link <- hazard_link(
                    module = tmp_module,
                    par_name = select[i],
                    contributions = paste0(select[i],  "*", select[i], link_contribution_prep(tmp_module))
                )

                link_out <- merge_link(link, link_out)
            }
            link_out
        }

    }
)
