#' Longitudinal Samples Storage
#'
#' This class is an extension of a standard `list` so that we
#' can define custom methods for it.
#'
#' @name LongitudinalSamples
#' @export
.LongitudinalSamples <- setClass(
    "LongitudinalSamples",
    contains = "list"
)

#' @rdname LongitudinalSamples
#'
#' @param x (`LongitudinalSamples`)\cr the samples object to subset.
#' @param i (`vector`)\cr the index vector.
#'
#' @return The subsetted `LongitudinalSamples` object.
#' @export
setMethod(
    f = "[",
    signature = "LongitudinalSamples",
    definition = function(x, i, ...) {
        res <- callNextMethod(x, i)
        .LongitudinalSamples(res)
    }
)
