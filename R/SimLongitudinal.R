# TODO - docs

#' @exportClass SimLongitudinal
.SimLongitudinal <- setClass(
    "SimLongitudinal",
    slots = list(
        times = "numeric"
    )
)

#' @export
SimLongitudinal <- function(times = seq(0, 100, 50)) {
    .SimLongitudinal(times = times)
}
