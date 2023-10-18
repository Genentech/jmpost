#' @include generics.R
#' @include Link.R
#' @include LongitudinalModel.R
NULL

# LinkNone-class ----

#' `LinkNone`
#'
#' This class extends the general [`Link`] class to specify no link
#' between the longitudinal and the time-to-event models.
#'
#' @exportClass LinkNone
.LinkNone <- setClass(
    Class = "LinkNone",
    contains = "Link"
)

# LinkNone-constructors ----

#' @rdname LinkNone-class
#'
#' @export
LinkNone <- function() {
    stan <- StanModule()
    .LinkNone(Link(stan = stan, name = "None"))
}

# addLink-LongitudinalModel,LinkNone ----

#' @rdname addLink
setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "LinkNone"),
    definition = function(x, y, ...) {
        x
    }
)


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "LinkNone",
    definition = function(object) {
        cat(as_print_string(object))
    }
)

#' @export
as_print_string.LinkNone <- function(object, ...) {
    return("No Link\n\n")
}
