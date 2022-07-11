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
        link_out <- hazard_link()

        for (i in 1:length(select)) {
            temp <- stan_module(
                functions = paste0(select[i], "_functions.stan"),
                prior = list("normal(0,5)"),
                inits = list(0)
            )
            names(temp@prior) <- select[i]
            names(temp@inits) <- select[i]

            link <- hazard_link(
                module = temp,
                par_name = select[i],
                contributions = paste0(select[i], "* rep_matrix(ttg(psi_ks, psi_kg, psi_phi), rows(time))")
            )

            link_out <- merge_link(link, link_out)
        }


        link_out
    }
)
