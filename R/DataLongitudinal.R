



.DataLongitudinal <- setClass(
    Class = "DataLongitudinal",
    representation = list(
        data = "data.frame",
        formula = "formula",
        subject = "character",
        threshold = "numeric"
    )
)

#' @export
DataLongitudinal <- function(data, formula, subject, threshold = NULL) {
    .DataLongitudinal(
        data = data,
        formula = formula,
        subject = subject,
        threshold = threshold
    )
}

setValidity(
    "DataLongitudinal",
    function(object) {
        if (length(object@subject) > 1) {
            return("`subject` should be a length 1 character vector or NULL")
        }
        if (!length(object@formula) == 3) {
            return("`formula` should be a 2 sided formula")
        }
        if (!length(object@formula[[3]]) == 1) {
            return("the rhs of `formula` should only have 1 value")
        }
        if (! object@subject %in% names(object@data)) {
            return("`subject` does not exist in `data`")
        }
        pt <- object@data[[object@subject]]
        if (!(is(pt, "character") | is(pt, "factor"))) {
            return("`data[[subject]]` should be of type character or factor")
        }
        return(TRUE)
    }
)


