

#' Merge
#'
#' Generic function to collapse two similar objects into a single combined object
#'
#' @param x An Object
#' @param y An Object to be merged with `x`
#' @export
setGeneric(
    "merge",
    function(x, y) standardGeneric("merge")
)


#' Parametrize TemplatedStanOs object with the selected Hazardlink
#' @param osmod TemplatedStanOs object
#' @param link HazardLink object
#' @export
setGeneric("parametrize", function(osmod, link) {
    standardGeneric("parametrize")
})
