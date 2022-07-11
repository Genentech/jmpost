#' @param link1
#' @param link2
#' @export

setGeneric("merge_link", def = function(link1, link2) {
    standardGeneric("merge_link")
})

# 3: can merge all StanAll objects with each other
setMethod(
    "merge_link",
    signature(link1 = "HazardLink", link2 = "HazardLink"),
    value = "HazardLink",
    def = function(link1, link2) {
        hazard_link(
            module = stan_module(
                functions = paste0(
                    paste0(link1@module@functions, collapse = "\n"),
                    paste0(link2@module@functions, collapse = "\n")
                ),
                prior = append(link1@module@prior, link2@module@prior),
                inits = append(link1@module@inits, link2@module@inits)
            ),
            par_name = c(link1@par_name, link2@par_name),
            contributions = paste0(link1@contributions, " + ", link2@contributions)
        )
    }
)
