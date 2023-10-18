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
#' @inheritParams stanmodel_arguments
#'
#' @export
LongitudinalModel <- function(
    stan = StanModule(),
    parameters = ParameterList(),
    name = "<Unnamed>",
    ...
) {

    base_long <- StanModule(
        x = "base/longitudinal.stan"
    )

    .LongitudinalModel(
        StanModel(
            stan = merge(base_long, stan),
            parameters = parameters,
            name = name,
            ...
        )
    )
}

#' @export
as_print_string.LongitudinalModel <- function(x, ...) {
    string <- sprintf(
        "\n%s Longitudinal Model Object with parameters:\n%s\n\n",
        x@name,
        paste("   ", as_print_string(x@parameters)) |> paste(collapse = "\n")
    )
    return(string)
}
