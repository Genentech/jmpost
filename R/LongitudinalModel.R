#' @include StanModel.R
NULL

# LongitudinalModel-class ----

#' `LongitudinalModel`
#'
#' This class extends the general [`StanModel`] class to comprise the longitudinal
#' model specification.
#'
#' @exportClass LongitudinalModel
.LongitudinalModel <- setClass(
    Class = "LongitudinalModel",
    contains = "StanModel"
)


# LongitudinalModel-constructors ----

#' @rdname LongitudinalModel-class
#'
#' @inheritParams stanmodule_arguments
#'
#' @export
LongitudinalModel <- function(stan = StanModule(),
                              parameters = ParameterList(),
                              ...) {

    base_long <- StanModule(
        x = "base/longitudinal.stan"
    )

    .LongitudinalModel(
        StanModel(
            stan = merge(base_long, stan),
            parameters = parameters,
            ...
        )
    )
}
