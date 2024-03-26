
# TODO - Docs

.Grid <- setClass("Grid")


.QuantityGenerator <- setClass(
    "QuantityGenerator",
    slots = c(
        "times" = "numeric",
        "subjects" = "character"
    )
)


.QuantityCollapser <- setClass(
    "QuantityCollapser",
    slots = c(
        "times" = "numeric",
        "groups" = "character",
        "indexes" = "list"
    )
)

setValidity(
    "QuantityCollapser",
    function(object) {
        if (
            length(object@times) != length(object@groups) ||
                length(object@times) != length(object@indexes)
        ) {
            return("Length of `times`, `groups`, and `indexes` must be equal")
        }
    }
)

#' @export
length.QuantityCollapser <- function(x) {
    length(x@indexes)
}
