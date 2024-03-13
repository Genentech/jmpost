
#' Abstract Simulation Class for Longitudinal Data
#'
#' @param times (`numeric`) the times to generate observations at.
#'
#' @description
#' This class exists to be extended by other classes that simulate longitudinal data.
#' It is not intended to be used directly.
#' @name SimLongitudinal-class
#' @family SimLongitudinal
#' @exportClass SimLongitudinal
.SimLongitudinal <- setClass(
    "SimLongitudinal",
    slots = list(
        times = "numeric"
    )
)

#' @rdname SimLongitudinal-class
#' @export
SimLongitudinal <- function(times = seq(0, 100, 50)) {
    .SimLongitudinal(times = times)
}
