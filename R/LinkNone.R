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
    .LinkNone(Link(stan = stan))
}

setMethod(
    f = "addLink",
    signature = c("LongitudinalModel", "LinkNone"),
    definition = function(x, y, ...) {
        x
    }
)

